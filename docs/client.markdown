---
layout: post
title:  "Client Side Application"
categories: jekyll update
permalink: client
---

## **Overview**

The client side application is written in Python and consists of four major components. These components are responsible for monitoring the file system, partitioning files, encrypting partitions, and maintaining consensus between the client application and the file system with the backend.

![Client Architecture](assets/client-daemon-chart.jpg){:class="img-responsive"}

**Client application components**:

- **Modification Daemon**: The daemon monitors for changes in the file system based on changes in file size and modification times by querying the CFJ as well as checking for new and deleted files. The watchdog Python library is used for querying the file system. Upon detecting a change in the file system, the daemon can invoke the partition service.
- **Partition Service:** Regular files are partitioned in chunks of 8 megabytes, who’s checksums are queried against partition information stored in CFJ. If a partition is changed, the client file journal is updated, the partition is encrypted, and the synchronizer is invoked.
    
    The partition service is also responsible for decrypting and reconstructing downloaded partitions retrieved by the syncing service.
    
- **Synchronization Daemon**: The synchronizer is responsible for maintaining consensus between the client and the backend. Modified data, file metadata, and artition information is uploaded and synced with the backed. The synchronizer is also listens for updates from the backend, updates the CFJ, deletes files, downloads partitions, and invokes the partition service accordingly.
- **Client File Journal (CFJ):** The CFJ is a SQLite database consisting of a file metadata table and a partition tables, responsible for storing file metadata, partition information, encryption keys,  and is more thoroughly detailed below.

## Client File Journal (CFJ)

The client file journal is a SQLite database consisting of a metadata and partitions table. This is allows for persistent data storage in case of application shutdowns. If corrupted or deleted, the CFJ can be quickly derived from the file system and the server file journal.

**Metadata table:**

The metadata table can be derived from the file system itself and is stored persistently for synchronization with the server file journal (SFJ).

| Field | Type | Notes |
| --- | --- | --- |
| file_id | string | Primary Key |
| path | string | Unique |
| file_type | bool |  |
| modification_time | timestamp | UTC timestamp |
| size | int | Size of file in bytes |

For the `file_type`  field,  `true` represents a regular file, `false` represents a directory, and `null` represents an unsupported or unknown file type (i.e. a socket, symbolic link, or other posix file type). In the future it may be smart to specify other file types, but as of now only regular files and directories are supported. While file modification time can be spoofed, any modifications will be caught when calculating checksums.

**Partitions table:**

The partitions table needs to be derived from the server metadata table.

| Field | Type | Notes |
| --- | --- | --- |
| partition_id | int | Primary Key |
| partition_salt | bytes | Randomly generated 64 bit sequence. |
| *next_id | int | Recursive foreign Key |
| *file_id | string | Foreign Key |
| *checksum | string | SHA-256 hash of partition contents+salt. |
| *symmetric_key | bytes | Randomly generated 128 bit AES-GCM symmetric key. |

Fields marked with `*`  are encrypted before being stored server side and decrypted client side with the users master key and the partition salt, with the symmetric key only being decrypted in memory when needed. The encryption/decryption processes is explained in further detail in the partition service section.

The `next_id` field is a recursive foreign key pointing to the next `partition_id` for a given file and is used for file reconstruction, with a value of `null` indicating the last partition of a file. The `checksum` field is a SHA-256 hash of the partition contents.

## Partition Service

The partition service can be broken down into two main applications, one for partitioning, analyzing, and encrypting files and another for decrypting and reconstructing files.

## File Partitioning and Encryption

Regular files are partitioned in chunks of 8 megabytes, who’s checksums are computed with a salted SHA-256 are queried against partition information stored in CFJ. If a partition is changed, the client file journal is updated, the partition is encrypted, and the synchronizer is invoked. For new files/partitions, a randomly generated salt is used to compute the checksum. Each partition encrypted with their own randomly generated 128 bit symmetric keys using AES-GCM before being synced with the server.

For directories, a singular partition of between 5kb to 50kb of random data is generated with file type marked in the file metadata. This does a good job of further masking the number of files and file types.

Partitions are stored inside of a `.safesync` hidden directory for staging partitions before being synced.

Partitions are only encrypted and decrypted on the client side to limit exposure outside of the client application.

## Partition Decryption and Reconstruction

Because files are split into multiple partitions, they need to be decrypted and reconstructed sequentially. 

For each partition, the symmetric key, checksum, and next partition id is decrypted using the user’s master key. The partition itself is decrypted with the symmetric key. A null partition id indicates the last partition of a file.

## Uploading

As described earlier, files are split into 8 megabyte chunks. A post request is sent to the synchronization server with the total number of partitions and the size of the last partition. Afterwards, each file is uploaded via HTTPs. Each file needs to be uploaded correctly to prevent data corruption and maintain consensus.

## Downloading

Downloading partitions is essentially the opposite of how uploading works. Initially, the client will send a request to the notification server asking for updates

## Challenges

The primary challenges involved with the client side application boil down to data integrity,  consensus, and effectively partitioning data. Other challenges include managing memory cost and bandwidth usage of the client side application. The CAP theorem is an important aspect to consider.

1. Data integrity
    1. Packet loss and dropped network connections can cause data loss and corrupt partitions. The expected number of partitions and checksums of the encrypted data should be sent along with each partition for validation.
2. Consensus
    1. Because files are split into separate partitions, it’s possible that an invalid combination of partitions can occur. This should be mitigated by computing checksums of each encrypted partition and validating file and partition versions in the partition metadata. File and partition versions need to match after a single partition update, with a fallback to the previous valid version.
    2. Multithreading and other asynchronous operations should be done in a way that effectively accounts for files updating constantly. Effectively utilizing locks to restrict the client application but not the user is important for this, as well as properly throwing and handling exceptions.
3. Cost
    1. Continuously reading and hashing files as well as encrypting data can have a significant impact on memory and be somewhat time consuming. Generally, for most computers and backend servers this shouldn’t be an issue. However, some care should still be taken.
    2. Working with native libraries (eg. System.Posix) to handle metadata is faster than manually computing sizes or keeping track of updates.
    3. Computing hashes lazily and working with files as streams of separate partitions of limited sizes can reduce memory usage. Loading in each and every file as a whole should be avoided.p

