--
-- File generated with SQLiteStudio v3.2.1
--
-- Text encoding used: UTF-8
--
PRAGMA foreign_keys = off;
BEGIN TRANSACTION;

-- Table: regular_file_metadata
CREATE TABLE regular_file_metadata (
    path          STRING  PRIMARY KEY
                          UNIQUE
                          NOT NULL
                          REFERENCES file_system (path) ON DELETE CASCADE NOT DEFERRABLE,
    size          INTEGER CHECK (size >= 0)
                          NOT NULL,
    modification_time TIMESTAMP,
    checksum      STRING  CONSTRAINT valid_checksum CHECK (length(checksum) = 64)
                          NOT NULL,
    symmetric_key STRING  CONSTRAINT valid_aes128_key CHECK (length(symmetric_key) = 32)
                          NOT NULL
);

-- Table: file_system
CREATE TABLE file_system (
    path              STRING  PRIMARY KEY
                              NOT NULL
                              UNIQUE,
    file_type         STRING CHECK (file_type IN ('f', 'd'))
);


COMMIT TRANSACTION;
PRAGMA foreign_keys = on;
