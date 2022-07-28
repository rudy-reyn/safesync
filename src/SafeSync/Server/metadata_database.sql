BEGIN;

CREATE TABLE IF NOT EXISTS public.partitions
(
    partition_id bigint NOT NULL,
    user_id bigint NOT NULL,
    salt character(16) COLLATE pg_catalog."default" NOT NULL,
    symmetric_key character(128) COLLATE pg_catalog."default" NOT NULL,
    next_id character varying COLLATE pg_catalog."default",
    checksum character varying COLLATE pg_catalog."default",
    CONSTRAINT partitions_pkey PRIMARY KEY (partition_id)
);

CREATE TABLE IF NOT EXISTS public.sync_times
(
    user_id bigint NOT NULL,
    sync_time timestamp without time zone,
    CONSTRAINT sync_times_pkey PRIMARY KEY (user_id)
);

CREATE TABLE IF NOT EXISTS public.metadata
(
    partition_id bigint NOT NULL,
    path character varying NOT NULL,
    modification_time character varying,
    PRIMARY KEY (partition_id)
);

ALTER TABLE IF EXISTS public.partitions
    ADD CONSTRAINT partitions_user_id_fkey FOREIGN KEY (user_id)
    REFERENCES public.sync_times (user_id) MATCH SIMPLE
    ON UPDATE NO ACTION
    ON DELETE NO ACTION
    NOT VALID;

ALTER TABLE IF EXISTS public.metadata
    ADD FOREIGN KEY (partition_id)
    REFERENCES public.partitions (partition_id) MATCH SIMPLE
    ON UPDATE NO ACTION
    ON DELETE NO ACTION
    NOT VALID;

CREATE VIEW preview AS
    SELECT
        partitions.partition_id, partitions.user_id,
        partitions.salt, partitions.symmetric_key,
        metadata.path, metadata.modification_time
    FROM
        partitions
        INNER JOIN metadata
        ON partitions.partition_id = metadata.partition_id
    WHERE
        partitions.next_id IS NULL;

END;
