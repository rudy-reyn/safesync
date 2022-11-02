# 11/02/22
# safesync/utils.py
from dataclasses import field

def hidden(*args, **kwargs):
    return field(*args, repr=False, **kwargs)
