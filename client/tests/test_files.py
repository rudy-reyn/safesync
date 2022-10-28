# tests/test_files.py
import pytest

def test_partition_stream(rand_NKb_stream):
    # with a 4 kilibyte chunk size
    assert len(list(rand_NKb_stream(kbs=2))) == 1
    assert len(list(rand_NKb_stream(kbs=6))) == 2
    assert len(list(rand_NKb_stream(kbs=10))) == 3
