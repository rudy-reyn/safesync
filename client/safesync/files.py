# files.py
from __future__ import annotations

from typing import (
    Optional, NewType, Self,
    Generator, IO, BinaryIO, TypeVar
)

from abc import abstractmethod, ABCMeta
from dataclasses import dataclass, field, KW_ONLY
from enum import Enum

import hashlib
from pathlib import Path

from safesync.utils import hidden
import safesync.crypto
from .crypto import Credentials

MEGABYTE = 2**20
UnixTime = NewType("UnixTime", float)
T = TypeVar("T")

@dataclass
class FSObject(metaclass=ABCMeta):
    """Base dataclass for FileObject and DirObject.

    Stores the path and the time of last modification.
    The base and sub classes are mainly used for the sake of pattern matching.
    """

    path: str
    mod_time: Optional[UnixTime] = None

    @classmethod
    @abstractmethod
    def from_path(cls, path: Path, **kwargs) -> Self:
        return

class DirObject(FSObject):
    @classmethod
    def from_path(cls, path: Path) -> Self:
        stats = path.stat()
        obj = cls(
            path = path.name,
            mod_time = stats.st_mtime,
        )
        return obj

@dataclass
class FileObject(FSObject):
    """Includes of generator of partitions that are read lazily."""

    _: KW_ONLY
    partitions: Generator[Partition]

    @classmethod
    def from_path(cls, path: Path, **kwargs) -> Self:
        stats = path.stat()
        file_object = cls(
            path = path.name,
            mod_time = stats.st_mtime,
            partitions = partition_file(path, **kwargs),
        )
        return file_object

@dataclass
class Partition:
    """Stores the checksum and a <= 4 megabyte block data of a partitioned file."

    The data and checksum are encrypted at uploaded time."""

    checksum: str
    data: bytes = hidden()

def partition_stream(stream: BinaryIO,
        chunk_size: int = 4 * MEGABYTE) -> Generator[Partition]:
    """Lazily reads a stream into 4 megabyte partitions and
    yields a Partition object while also generating salts and keys.

    Paths are encrypted only if they need to be uploaded"""

    while data := stream.read(chunk_size):
        yield Partition(
            checksum = sha256_digest(data),
            data = data
        )

def partition_file(path: Path | str, *args, **kwargs) -> Generator[Partition]:
    # Using a generator will automatically close the file.
    with open(path, "rb") as file:
        yield from partition_stream(file, *args, **kwargs)

@dataclass
class _Encrypted:
    _: KW_ONLY
    credentials: Credentials = field(default_factory=Credentials)

@dataclass
class EncryptedFile(FileObject, _Encrypted):
    pass

@dataclass
class EncryptedDir(DirObject, _Encrypted):
    pass

@dataclass
class EncryptedPartition(Partition, _Encrypted):
    pass

def encrypt_fsobj(fs_obj: T, credentials: Optional[Credentials] = None) -> T:
    if not credentials:
        credentials = Credentials()

    match fs_obj:
        case DirObject(p, mt):
            return EncryptedDir(p, mt, credentials=creds)
        case FileObject(p, mt, parts):
            enc_parts = map(encrypt_partition, parts)
            return EncryptedFile(p, mt, enc_parts, credentials=creds)
        case FSObject(_, _):
            raise ValueError("Can't directly instantiate a base FSObject.")
        case _:
            raise ValueError("Not a file system object.")

def encrypt_partition(p: Partition,
            creds: Optional[Credentials] = None) -> EncryptedPartition:
    if not creds:
        creds = Credentials()

    return EncryptedPartition(p.checksum, p.data, creds)

def sha256_digest(data: bytes) -> bytes:
    return hashlib.sha256(data).hexdigest()

FileType = Enum(" FileType", ("dir", "file", "unknown"))
