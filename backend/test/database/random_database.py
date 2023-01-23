# 01/20/23
# random_database.py
from typing import Optional

import string
import random
import uuid
from uuid import uuid4, UUID
from dataclasses import dataclass, astuple

@dataclass
class Metadata:
    headId: UUID
    ownerId: int
    salt: str
    path: str
    modtime: str

class Null:
    def __repr__(self):
        return "null"
null = Null()

@dataclass
class Partition:
    partitionId: UUID
    nextId: Optional[UUID]
    salt: str
    checksum: str

def random_string(size=16, alphabet=string.ascii_letters + string.digits + string.punctuation):
    return "".join(random.choice(alphabet) for _ in range(size))

def random_salt():
    return random_string()


def random_file(amount=1, ownerId=0):
    current_uuid = str(uuid4())
    metadata = Metadata(current_uuid, ownerId, random_salt(), random_string(10), random_string(32))

    partitions = []
    for i in range(amount):
        if i == amount - 1:
            next_uuid = null
        else:
            next_uuid = str(uuid4())
        partition = Partition(current_uuid, next_uuid, random_salt(), random_string(64, string.ascii_letters))
        partitions.append(partition)
        current_uuid = next_uuid
    return metadata, partitions

def make_sql(user: User, metadata: Metadata, partitions: list[Partition]) -> str:
    sql = f"INSERT INTO metadata VALUES {astuple(metadata)}\n;\n"
    sql += "INSERT INTO partitions VALUES\n"
    for part in partitions:
        sql += repr(astuple(part)) + "\n"
    return sql

def random_user():
    return random_string(random_string(32)), random_salt(), )

if __name__ == "__main__":
    for i in range

    print(to_sql(*random_file(5)))

