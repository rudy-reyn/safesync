# 10/24/22
# client_file_journal.py
import sqlalchemy
from sqlalchemy.orm import declarative_base, relationship
from sqlalchemy import (
    Column, ForeignKey, Enum,
    Integer, String,
)
import files

Base = declarative_base()

# Used to create new instances
def UUIDColumn(*args, **kwargs):
    return Column(String(36), *args, **kwargs)

class MetadataTable(Base):
    __tablename__ = "metadata"

    file_id = UUIDColumn(primary_key=True)
    path = Column(String, primary_key=True)
    file_type = Column(Enum(files.FileType), nullable=False)
    mod_time = Column(Integer)
    size = Column(Integer)

class PartitionsTable(Base):
    partition_id = UUIDColumn(primary_key=True)
    partition_salt = Column(String(8), nullable=False)
    file_id = UUIDColumn(ForeignKey("metadata.file_id"), nullable=False)
    checksum: Column(String(64), nullable=False)
    symmetric_key = Column(String(32))
    next_id: Optional[UUID_bytes] = None
