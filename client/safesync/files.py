# files.py
from __future__ import annotations
from typing import Optional, NewType, Self, Generator, IO, BinaryIO
from enum import Enum
from dataclasses import dataclass, field, KW_ONLY
from abc import abstractmethod, ABCMeta

import hashlib
from pathlib import Path

import safesync.crypto
from .crypto import Credentials

MEGABYTE = 2**20
UnixTime = NewType("UnixTime", float)

def hidden(*args, **kwargs):
    return field(*args, repr=False, **kwargs)

@dataclass
class Partition:
    """Stores the checksum and a <= 4 megabyte block data of a partitioned file."

    The data and checksum are encrypted at uploaded time."""

    checksum: str
    data: bytes = hidden()

@dataclass
class FileSystemObject(metaclass=ABCMeta):
    """Base dataclass for FileObject and DirObject.

    Stores the path and the time of last modification.
    The base and sub classes are mainly used for the sake of pattern matching."""

    path: str
    mod_time: Optional[UnixTime] = None

    @classmethod
    @abstractmethod
    def from_path(cls, path: Path, **kwargs) -> Self:
        return

@dataclass
class FileObject(FileSystemObject):
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

class DirObject(FileSystemObject):
    @classmethod
    def from_path(cls, path: Path) -> Self:
        stats = path.stat()
        obj = cls(
            path = path.name,
            mod_time = stats.st_mtime,
        )
        return obj

@dataclass
class _Encrypted:
    _: KW_ONLY
    credentials: Credentials = field(default_factory=Credentials)

@dataclass
class EncryptedFileObject(FileObject, _Encrypted):
    @classmethod
    def from_file_object(cls, file_object, master_key):
        creds = Credentials()

        encrypted_partitions = (
            encrypt_partition(partition, creds.key)
            for partition in file_object.partitions
        )

        encrypt_file_object = cls(
            path = crypto.encrypt(path, *creds),
            mod_time = file_object.mod_time,
            partitions = encrypted_partitions,
            credentials = creds.encrypt_key(master_key)
        )
        return encrypt_file_object

@dataclass
class EncryptedDirObject(DirObject, _Encrypted):
    @classmethod
    def from_dir_object(cls, obj, master_key):
        creds = Credentials()
        enc = cls(
            path = crypto.encrypt(obj.path, *creds),
            mod_time = obj.mod_time,
            credentials = creds.encrypt_key(master_key)
        )
        return enc

@dataclass
class EncryptedPartition(Partition, _Encrypted):
    @classmethod
    def from_partition(cls, part, parent_key) -> Self:
        creds = Credentials()

        encrypted_partition = cls(
            checksum = crypto.encrypt(part.checksum.encode(), *creds),
            data  = crypto.encrypt(part.data, *creds),
            credentials = creds.encrypt_key(parent_key)
        )
        return encrypted_partition

encrypt_partition = EncryptedPartition.from_partition

def partition_stream(stream: BinaryIO,
        chunk_size: int = 4 * MEGABYTE) -> Generator[Partition]:
    """Lazily reads a stream into 4 megabyte partitions and
    yields a Partition object while also generating salts and keys.

    Paths are encrypted only if they need to be uploaded"""

    while data := stream.read(chunk_size):
        partition = Partition(
            checksum = sha256_digest(data),
            data = data
        )
        yield partition

def partition_file(path: Path | str, *args, **kwargs) -> Generator[Partition]:
    # Using a generator will automatically close the file.
    with open(path, "rb") as file:
        yield from partition_stream(file, *args, **kwargs)

def enc_partition_stream(stream: BinaryIO, *args, **kwargs) -> Generator[EncryptedPartition]:
    return map(encrypt_partition, partition_stream(stream, *args, **kwargs))

def enc_file_partitions(path: Path | str, *args, **kwargs) -> Generator[EncryptedPartition]:
    return map(encrypt_partition, partition_file(path, *args, **kwargs))

def sha256_digest(data: bytes) -> bytes:
    return hashlib.sha256(data).hexdigest()

FileType = Enum(" FileType", ("dir", "file", "unknown"))
