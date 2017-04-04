-- Create the schema for the entire api database. Only run this file once,
-- it will overrwrite previous data.

--------------------------------------------------------------------------------
-- FUNCTIONS

CREATE OR REPLACE FUNCTION add_post_time() RETURNS trigger AS $$
       BEGIN
            NEW.post_date := now();
            RETURN NEW;
        END
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION add_post_url() RETURNS trigger AS $$
       BEGIN
            NEW.post_url := replace((lower(trim(NEW.post_title))), ' ', '-');
            RETURN NEW;
       END
$$ LANGUAGE plpgsql;

--------------------------------------------------------------------------------
-- POSTS

CREATE TABLE posts (
       post_id SERIAL PRIMARY KEY,
       post_date TIMESTAMP WITH TIME ZONE, 
       post_title TEXT,
       post_body TEXT,
       post_url TEXT
);

CREATE TRIGGER posts_insert_date
       BEFORE INSERT ON posts
       FOR EACH ROW
       EXECUTE PROCEDURE add_post_time();

CREATE TRIGGER posts_insert_url
       BEFORE INSERT ON posts
       FOR EACH ROW
       EXECUTE PROCEDURE add_post_url();

CREATE TRIGGER posts_insert_url
       BEFORE UPDATE ON posts
       FOR EACH ROW
       EXECUTE PROCEDURE add_post_url();

-- Example, to fetch all post id's
-- WITH tags AS ( SELECT tag, post_id FROM post_tags WHERE tag="Hello World" )
-- SELECT post_id,
--        post_title,
--        post_body,
--        post_date,
--        (SELECT tag FROM tags) AS post_tags
-- FROM posts
-- WHERE post_id IN (SELECT post_id FROM tags);
CREATE TABLE post_tags (
       tag VARCHAR(50),
       post_id INTEGER REFERENCES posts(post_id)
);

--------------------------------------------------------------------------------
-- USERS

CREATE TABLE users (
       username VARCHAR(40),
       token TEXT
);
