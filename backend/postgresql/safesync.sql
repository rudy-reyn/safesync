
CREATE TABLE users
    ( userId bigserial PRIMARY KEY
    , masterKey character(32) NOT NULL
    , keySalt character(16) NOT NULL
    , syncTime timestamp without time zone DEFAULT now()
);

CREATE TABLE partitions
    ( partitionId uuid PRIMARY KEY
    , nextId uuid REFERENCES partitions(partitionId) UNIQUE
    , salt character(16) NOT NULL
    , checksum character(64) NOT NULL
);

CREATE TABLE metadata
	( metadataId bigserial PRIMARY KEY 
    , ownerId bigint REFERENCES users(userId) NOT NULL
    , headId uuid REFERENCES partitions(partitionId) UNIQUE 
    , salt character(16) NOT NULL
    , path text NOT NULL
    , modTime character(32)
);

CREATE TABLE auth
	( userId bigint PRIMARY KEY REFERENCES users(userId)
	, email varchar(320) UNIQUE
	, passwordHash character(32) NOT NULL
	, passwordSalt character(16) NOT NULL
	)
;

CREATE VIEW files AS
	SELECT
		metadata.headId,
		metadata.ownerId,
		metadata.salt,
		metadata.path,
		metadata.modtime,
		(
			WITH RECURSIVE pIds as (
				SELECT p1.partitionId, p1.nextId
				FROM partitions as p1
				WHERE p1.partitionId = metadata.headId
				UNION ALL
				SELECT p1.partitionId, p1.nextId
				FROM partitions as p1, pIds
				WHERE p1.partitionId = pIds.nextId
			)
			SELECT array_agg(partitionId) FROM pIds
		) as partitionIds
	FROM
		metadata
;

-- INSERT INTO users
-- 	(masterKey, keySalt)
-- VALUES
-- 	('M0NnTVMzZUF8S3lEQCs4KDA0YjYxOT99', 'eV87anwvOThrZTVP'),
-- 	('LjRUYyl2KGNeSzc1NTg9biI5ZGUyTTlG', 'JEhJPTVwLyw0N0ow')
-- ;

-- INSERT INTO partitions
-- VALUES
-- ('c92a4f99-c2ef-45fe-8880-a2c1d3325bd7', '15ca39d7-5d03-412f-ac4e-12daf562ba64', 'vsdfI0~|chYU6snY', concat(md5(random()::text), md5(random()::text))),
-- ('15ca39d7-5d03-412f-ac4e-12daf562ba64', 'cc8c3dd3-02c4-4d75-a6f4-d4a1980fda37', '=l4kI0&3cb+76sA_', concat(md5(random()::text), md5(random()::text))),
-- ('cc8c3dd3-02c4-4d75-a6f4-d4a1980fda37', '570c1f32-ac59-44e9-850b-8f752fde9572', 'b4_fN=Bc738MornY', concat(md5(random()::text), md5(random()::text))),
-- ('570c1f32-ac59-44e9-850b-8f752fde9572', null, 'q#4kI0~|{9076snY', concat(md5(random()::text), md5(random()::text))),

-- ('72b49b08-ff7f-47b7-9794-a3fca7f690db', 'b1e928ff-daf4-446f-8f5f-073cfae59adb', '=-!u{9&-chYU6snY', concat(md5(random()::text), md5(random()::text))),
-- ('b1e928ff-daf4-446f-8f5f-073cfae59adb', null, 'cu$bc-cu{9076snY', concat(md5(random()::text), md5(random()::text))),

-- ('1F995669-135F-4B85-97F8-27161045F7DD', null, 'QTZoNjd7KHI0ZUhd', concat(md5(random()::text), md5(random()::text)))
-- ;

-- INSERT INTO metadata
-- VALUES
-- 	('c92a4f99-c2ef-45fe-8880-a2c1d3325bd7', 1, substr(md5(random()::text), 0, 16), '-', md5(random()::text)),
-- 	('72b49b08-ff7f-47b7-9794-a3fca7f690db', 1, substr(md5(random()::text), 0, 16), '-', md5(random()::text)),
-- 	('1F995669-135F-4B85-97F8-27161045F7DD', 2, substr(md5(random()::text), 0, 16), '-', md5(random()::text))
-- ;

-- SELECT * FROM files where ownerId = 1;