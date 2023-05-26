DROP TABLE IF EXISTS user_info_db CASCADE;
CREATE TABLE IF NOT EXISTS user_info_db(
    full_name VARCHAR(35),
    email_address VARCHAR(320), /* 320 characters is the max length of an email address */
    password VARCHAR(25),
    PRIMARY KEY(full_name)
);

DROP TABLE IF EXISTS user_problems CASCADE;
CREATE TABLE IF NOT EXISTS user_problems(
    problem_type VARCHAR(35),
    numeral_a INT,
    operator VARCHAR(1), 
    numeral_b INT,
    answer INT,
    correct INT,  /* number of problems correct, to be incremented when user submits correct answer */
    attempted INT /* number of problems attempted, to be incremented when user submits any answer */
    -- CONSTRAINT user_full_name FOREIGN KEY(full_name) REFERENCES user_info_db(full_name) 
);
