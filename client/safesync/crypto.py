# crypto.py
# Uses the cryptography library found at https://cryptography.io
from typing import Self
from dataclasses import dataclass, field

import os
import secrets

from cryptography.hazmat.primitives.ciphers import Cipher, modes
from cryptography.hazmat.primitives.ciphers.aead import AESGCM

def nonce96():
    return os.urandom(12)

def generate_bytes(size=16, *args, **kwargs):
    return secrets.token_bytes(size, *args, **kwargs)

@dataclass
class Credentials:
    key: bytes = field(repr=False, default_factory=generate_bytes) # 128 bit AES-GCM key
    salt: bytes = field(repr=False, default_factory=generate_bytes) # 128 bit salt
    nonce: int = field(repr=False, default_factory=nonce96)

    def encrypt_key(self, master_key) -> Self:
        return Credentials(encrypt(self.key, master_key, self.salt, self.nonce))

    def __iter__(self):
        yield from (self.key, self.salt, self.nonce)

def encrypt(data: bytes, key: bytes, salt: bytes, nonce: int) -> bytes:
    return data
