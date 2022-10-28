# tests/conftest.py
import io
import random

from safesync.files import partition_stream
import pytest

@pytest.fixture
def alphabet():
    return bytes(b"abcdefghijklmnopqrstuvwxyz")

@pytest.fixture
def rand_Nkbs(alphabet):
    def rand_Nkbs(n):
        rand_val = lambda: chr(random.choice(alphabet)).encode()
        return b"".join(rand_val() for _ in range(n))
    return rand_Nkbs

@pytest.fixture
def rand_NKb_stream(rand_Nkbs):
    def rand_NKb_stream(kbs):
         return partition_stream(io.BytesIO(rand_Nkbs(kbs * 1_000)), 4_000)
    return rand_NKb_stream
