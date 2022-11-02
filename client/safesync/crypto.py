# crypto.py
# Uses the cryptography library found at https://cryptography.io
from __future__ import annotations
from typing import Self
from dataclasses import dataclass, astuple

import os
import secrets

from cryptography.hazmat.primitives.ciphers import Cipher, modes
from cryptography.hazmat.primitives.ciphers.aead import AESGCM
from cryptography.hazmat.primitives.kdf.scrypt import Scrypt

# from safesync.utils import hidden
from .utils import hidden

@dataclass
class EncryptedData:
    data: bytes = hidden()

    @classmethod
    def encrypt(cls, data: bytes, credentials: Credentials) -> Self:
        return AESGCM(*credentials)

def nonce96():
    return os.urandom(12)

def generate_bytes(size=16, *args, **kwargs):
    return secrets.token_bytes(size, *args, **kwargs)

@dataclass
class Credentials:
    key: bytes = hidden(default_factory=generate_bytes) # 128 bit AES-GCM key
    salt: bytes = hidden(default_factory=generate_bytes) # 128 bit salt
    nonce: int = hidden(default_factory=nonce96)

    def __iter__(self):
        yield from astuple(aself)

class Key:
    def __init__(self, key: bytes, salt: bytes, length=32, n=2**14, r=8, p=1):

        self.scrypt = Scrypt(salt=salt, length=length, n=n, r=r, p=p)
        self._key: bytes | None = None

    def __repr__(self):
        name = type(self).__name__
        length = self.scrypt._length
        r = self.scrypt._r
        p = self.scrypt._p
        for i in range(32):
            if self.scrypt._n == 2**i:
                n = f"2**{i}"
                break
        else:
            n = self.scrypt._n
        return f"{name}(key=<bytes>, salt=<bytes>, {length=}, n={n}, {r=}, {p=})"

    @property
    def key(self) -> bytes:
        # This is evaluated lazily
        if self._key is None:
            self._key = self.scrypt.derive(self.key)
        return self._key

def encrypt(*args, **kwargs) -> EncryptedData:
    return EncryptedData.encrypt(*args, **kwargs)

if __name__ == "__main__":
    print(Key(b"key", b"salt"))
