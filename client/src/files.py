# files.py
from __future__ import annotations
from typing import Optional, NewType, Self, Generator, IO, BinaryIO
from enum import Enum
from dataclasses import dataclass, field, KW_ONLY

import io
import hashlib
import secrets
from pathlib import Path

import crypto

MEGABYTE = 2**20
UTCTime = NewType("UTCTime", float)

@dataclass
class Partition:
    # checksum of the unencrypted data, is encrypted before being uploaded server side.
    checksum: salt
    # Data is encrypted later.
    data: bytes = field(repr=False)

@dataclass
class _BaseObject:
    path: str
    mod_time: Optional[UTCTime] = None

@dataclass
class FileObject(_BaseObject):
    _: KW_ONLY
    partitions: Generator[Partition]

    @classmethod
    def from_path(cls, path: Path, *, chunk_size: int = 4 * MEGABYTE) -> Self:
        stats = path.stat()
        file_object = cls(
            path = path.name,
            mod_time = stats.st_mtime,
            partitions = partition_file(path, chunk_size),
        )
        return file_object

class DirObject(_BaseObject):

    @classmethod
    def from_path(cls, path: Path) -> Self:
        stats = path.stat()
        dir_object = cls(
            path = path.name,
            mod_time = stats.st_mtime,
        )
        return dir_object

@dataclass
class _Encrypted:
    _: KW_ONLY
    salt: bytes = field(repr=False) # 128 bit salt
    key: bytes = field(repr=False) # 128 bit AES-GCM key, secrets.token_urlsafe

@dataclass
class EncryptedFileObject(FileObject, _Encrypted):

    @classmethod
    def from_file_object(cls, file_object, master_key):
        fobj_key = secrets.token_urlsafe(32),
        salt = secrets.token_urlsafe(32),

        encrypted_partitions = (
            encrypt_partition(partition, fobj_key)
            for partition in file_object.partitions
        )
        encrypt_file_object = cls(
            path = crypto.encrypt(path, fobj_key, salt),
            mod_time = file_object.mod_time,
            partitions = encrypted_partitions,
            salt = salt,
            key = crypto.encrypt(fobj_key, salt, master_key)
        )
        return encrypt_file_object

@dataclass
class EncryptedDirObject(DirObject, _Encrypted):

    @classmethod
    def from_dir_object(cls, dir_object, master_key):
        dobj_key = secrets.token_urlsafe(32),
        salt = secrets.token_urlsafe(32),
        encrypt_dir_object = cls(
            path = crypto.encrypt(path, dobj_key, salt),
            mod_time = dir_object.mod_time,
            salt = salt,
            key = crypto.encrypt(dobj_key, salt, master_key)
        )
        return encrypt_dir_object

@dataclass
class EncryptedPartition(Partition, _Encrypted):

    @classmethod
    def from_partition(cls, partition, parent_key) -> Self:
        key = secrets.token_urlsafe(32),
        salt = secrets.token_urlsafe(32),
        encrypted_partition = cls(
            checksum = crypto.encrypt(partition.checksum, key, salt),
            salt = salt,
            key  = crypto.encrypt(key, parent_key, salt),
            data = crypto.encrypt(partition.data, key, salt)
        )
        return encrypted_partition

def sha256_digest(data: bytes) -> bytes:
    return hashlib.sha256(data).hexdigest()

def partition_stream(stream: BinaryIO,
        partition_size: int = 4 * MEGABYTE) -> Generator[Partition]:
    """Lazily reads a stream into 4 megabyte partitions and
    yields a Partition object while also generating salts and keys.

    Paths are encrypted only if they need to be uploaded"""

    while data := stream.read(partition_size):
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
    for partition in partition_stream(stream):
        yield EncryptedPartition.from_partition(partition)

def enc_file_partitions(path: Path | str, *args, **kwargs):
    with open(path, "rb") as file:
        yield from enc_partition_stream(stream)

def encrypt_partition(*args, **kwargs):
    return EncryptedPartition.from_partition(*args, **kwargs)

FileType = Enum(" FileType", ("dir", "file", "unknown"))

if __name__ == "__main__":
    path = Path("daemon.py")
    file_object = FileObject.from_path(path, chunk_size=4 * MEGABYTE)

    print(file_object)
    for partition in file_object.partitions:
        encrypted = EncryptedPartition.from_partition(partition, None)
