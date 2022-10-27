# files.py
from __future__ import annotations
from typing import Optional, NewType, Self
from enum import Enum, auto
from dataclasses import dataclass

import uuid
import hashlib
from pathlib import Path

HASH = hashlib._hashlib.HASH
Id = NewType("Id", str)
UTCTime = NewType("UTCTime", int)

@dataclass
class Metadata:
    file_id: Id
    path: str
    file_type: FileType
    mod_time: Optional[UTCTime] = None
    size: Optional[int] = None

    @classmethod
    def from_path(cls, file: Path) -> Optional[Self]:
        if not file.exists():
            return
        file_type = get_file_type(file)
        mod_time = None
        size = None
        if file_type == FileType.regular:
            stats = Path.stat()
            mod_time = stats.st_mtime
            size = stats.st_size
        metadata = Metadata(
            str(uuid.uuid4()), file.name,
            file_type, mod_time, size
        )
        return metadata

    @classmethod
    def from_path_name(cls, path_name: str) -> Optional[Self]:
        return cls.from_path(Path(path_name))

def generate_id() -> str:
    return str(uuid.uuid4())

@dataclass
class Partition:
    partition_id: Id
    partition_salt: bytes
    file_id: Id
    checksum: bytes
    symmetric_key: str  # 128 bit AES-GCM key, generated with secrets.token_urlsafe(32)
    next_id: Optional[UUID_bytes] = None

FileType = Enum(" FileType", ("directory", "regular", "unknown"))

def get_filetype(file: Path) -> Optional[FileType]:
    if not file.exists:
        return None
    if file.is_dir():
        return FileType.directory
    if file.is_file():
        return FileType.regular
    return file.unknown
