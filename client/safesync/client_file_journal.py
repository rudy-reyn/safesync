# client_file_journal.py
import sqlalchemy
from sqlalchemy.orm import declarative_base, relationship
from sqlalchemy import (
    Column, ForeignKey, Enum,
    Integer, String,
    select
)
from sqlalchemy.sql.expression import Select

import safesync.files

Base = declarative_base()

def PrimaryKey(col_type=Integer, /, *args, **kwargs):
    return Column(col_type, *args, primary_key=True, **kwargs)

def StringNN(size=32, /, *args, **kwargs):
    return Column(String(size), *args, nullable=False, **kwargs)

# Used to create new instances
class Metadata(Base):
    __tablename__ = "metadata"

    # file_id is the same as the path
    file_id = PrimaryKey(String)
    file_type = Column(Enum(files.FileType), nullable=False)
    mod_time = Column(Integer)
    size = Column(Integer)
    salt = StringNN(32)
    symmetric_key = StringNN(32)

class Partitions(Base):
    file_id = PrimaryKey(String, ForeignKey("metadata.path"))
    partition_id = PrimaryKey()
    checksum = StringNN(64)
    salt = StringNN(32)
    symmetric_key = StringNN(32)

def lookup_file(path) -> Select:
    return select(Metadata).where(Metadata.path == path)
