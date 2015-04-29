CREATE TABLE users (
  id    serial NOT NULL,
  name  varchar NOT NULL,
  email varchar NOT NULL
);

ALTER TABLE users ADD PRIMARY KEY (id);
