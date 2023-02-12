DROP TABLE IF EXISTS auth;
DROP TABLE IF EXISTS partitions CASCADE;
DROP TABLE IF EXISTS objects;
DROP TABLE IF EXISTS users;

CREATE TABLE users
    ( id bigserial PRIMARY KEY
    , user_id uuid UNIQUE NOT NULL
    , email varchar(320) UNIQUE
    , phone char(15) UNIQUE
    , password character(32) NOT NULL
    , password_salt character(16) NOT NULL
    , master_key character(32) NOT NULL
    , key_salt character(16) NOT NULL
    , sync_time timestamp without time zone DEFAULT now()
);

CREATE TABLE partitions
    ( id bigserial PRIMARY KEY
    , partition_id uuid UNIQUE
);

CREATE TABLE objects
	( id bigserial PRIMARY KEY 
	, object_id uuid UNIQUE NOT NULL
    , owner_id uuid REFERENCES users(user_id) NOT NULL
    , head_id uuid REFERENCES partitions(partition_id) UNIQUE 
    , uploaded_at timestamp without time zone DEFAULT now()
    , salt character(16) NOT NULL
    , path text NOT NULL
    , mod_time timestamp without time zone
);

ALTER TABLE partitions
    ADD COLUMN object_id uuid REFERENCES objects(object_id),
    ADD COLUMN next_id uuid REFERENCES partitions(partition_id) UNIQUE,
    ADD COLUMN salt character(16) NOT NULL,
    ADD COLUMN checksum character(64) NOT NULL,
    ADD COLUMN created_at timestamp without time zone DEFAULT now(),
    ADD COLUMN updated_at timestamp without time zone
;

CREATE VIEW user_partitions AS
    SELECT
        objects.user_id,
        objects.object_id,
        partitions.partition_id
    FROM
        objects, partitions
    WHERE
        objects.object_id = partitions.partition_id
;

-- CREATE VIEW objects AS
-- 	SELECT
-- 		metadata.head_id,
-- 		metadata.owner_id,
-- 		metadata.salt,
-- 		metadata.path,
-- 		metadata.mod_time,
-- 		(
-- 			WITH RECURSIVE pIds as (
-- 				SELECT p1.partition_id, p1.next_id
-- 				FROM partitions as p1
-- 				WHERE p1.partition_id = metadata.head_id
-- 				UNION ALL
-- 				SELECT p1.partition_id, p1.next_id
-- 				FROM partitions as p1, pIds
-- 				WHERE p1.partition_id = pIds.next_id
-- 			)
-- 			SELECT array_agg(partition_id) FROM pIds
-- 		) as partitionIds
-- 	FROM
-- 		metadata
-- ;

INSERT INTO users
	(user_id, master_key, key_salt)
VALUES

	('e1268c35-6988-4d2f-b459-49b22f37d797', 'M0NnTVMzZUF8S3lEQCs4KDA0YjYxOT99', 'eV87anwvOThrZTVP'),
	('94deec70-49f5-4c0d-a0ed-b5316c22c29d', 'LjRUYyl2KGNeSzc1NTg9biI5ZGUyTTlG', 'JEhJPTVwLyw0N0ow')
;

INSERT INTO objects (object_id, owner_id, head_id, salt, path, mod_time)
VALUES
	('c92a4f99-c2ef-45fe-8880-a2c1d3325bd7', 'e1268c35-6988-4d2f-b459-49b22f37d797', null, substr(md5(random()::text), 0, 16), '-', null),
	('72b49b08-ff7f-47b7-9794-a3fca7f690db', 'e1268c35-6988-4d2f-b459-49b22f37d797', null, substr(md5(random()::text), 0, 16), '-', null),
	('1F995669-135F-4B85-97F8-27161045F7DD', '94deec70-49f5-4c0d-a0ed-b5316c22c29d', null, substr(md5(random()::text), 0, 16), '-', null)
;

INSERT INTO partitions (partition_id, object_id, next_id, salt, checksum)
VALUES
('41085f9c-b7eb-46fe-9f91-6f99c0fcebf1', 'c92a4f99-c2ef-45fe-8880-a2c1d3325bd7', '15ca39d7-5d03-412f-ac4e-12daf562ba64', 'vsdfI0~|chYU6snY', concat(md5(random()::text), md5(random()::text))),
('15ca39d7-5d03-412f-ac4e-12daf562ba64', 'c92a4f99-c2ef-45fe-8880-a2c1d3325bd7', 'cc8c3dd3-02c4-4d75-a6f4-d4a1980fda37', '=l4kI0&3cb+76sA_', concat(md5(random()::text), md5(random()::text))),
('cc8c3dd3-02c4-4d75-a6f4-d4a1980fda37', 'c92a4f99-c2ef-45fe-8880-a2c1d3325bd7', '570c1f32-ac59-44e9-850b-8f752fde9572', 'b4_fN=Bc738MornY', concat(md5(random()::text), md5(random()::text))),
('570c1f32-ac59-44e9-850b-8f752fde9572', 'c92a4f99-c2ef-45fe-8880-a2c1d3325bd7', null, 'q#4kI0~|{9076snY', concat(md5(random()::text), md5(random()::text))),

('b3036536-cd16-460e-8d10-766654c8fe1b', '72b49b08-ff7f-47b7-9794-a3fca7f690db', 'b1e928ff-daf4-446f-8f5f-073cfae59adb', '=-!u{9&-chYU6snY', concat(md5(random()::text), md5(random()::text))),
('b1e928ff-daf4-446f-8f5f-073cfae59adb', '72b49b08-ff7f-47b7-9794-a3fca7f690db', null, 'cu$bc-cu{9076snY', concat(md5(random()::text), md5(random()::text))),

('f85552a4-2245-4e8f-abaa-08c729fda8d9', '1F995669-135F-4B85-97F8-27161045F7DD', null, 'QTZoNjd7KHI0ZUhd', concat(md5(random()::text), md5(random()::text)))
;

UPDATE objects
SET head_id = '41085f9c-b7eb-46fe-9f91-6f99c0fcebf1'
WHERE object_id = 'c92a4f99-c2ef-45fe-8880-a2c1d3325bd7';

UPDATE objects
SET head_id = 'b3036536-cd16-460e-8d10-766654c8fe1b'
WHERE object_id = '72b49b08-ff7f-47b7-9794-a3fca7f690db';

UPDATE objects
SET head_id = 'f85552a4-2245-4e8f-abaa-08c729fda8d9'
WHERE object_id = '1F995669-135F-4B85-97F8-27161045F7DD';