DELETE FROM sources WHERE id = 'ae22346f-a0f2-4c1f-8db3-c1ff889cc03d';

DROP TABLE sources;

CREATE TABLE sources (
    id          uuid primary key,
    title       varchar(256) NOT NULL,
    description text,
    url         varchar(256) NOT NULL,
    created_at  timestamp with time zone not null, 
    updated_at  timestamp with time zone not null
);



