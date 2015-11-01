# Web App. Testing experimentation

## Prepare

* Redis
* MySQL
* MailCatcher
* SQLite

```sql
-- $ mysql -u root -p

CREATE DATABASE main DEFAULT CHARACTER SET utf8mb4;
CREATE TABLE main.user (
    id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(64) NOT NULL,
    email_address VARCHAR(128) NOT NULL,
    created_at DATETIME NOT NULL,
    last_loggedin_at DATETIME NOT NULL,
    UNIQUE KEY unique_user_name (name),
    UNIQUE KEY unique_user_email_address (email_address)
);
CREATE USER 'tester'@"%" IDENTIFIED BY 'tester';
GRANT ALL PRIVILEGES ON main.* TO 'tester'@"%";
```

```sql
-- $ sqlite3 test.db

CREATE TABLE user (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name VARCHAR(64) NOT NULL UNIQUE,
    email_address VARCHAR(128) NOT NULL UNIQUE,
    created_at DATETIME NOT NULL,
    last_loggedin_at DATETIME NOT NULL
);
```

SQLite uses "main" as a virtual database name. I don't know how to rename it.
Thus both MySQL and SQLite use "main" as a database name here.

## Build & Run

```sh
$ git submodule update
$ cabal sandbox init && cabal sandbox add-source hdbc-mysql/
$ cabal build
$ cabal run server
```

## Check

```sh
$ curl -X POST \
    -H 'Content-Type: application/json' \
    -d '{"reg_name":"tester","reg_email_address":"tester@localhost"}' \
    http://localhost:8081/register

$ curl -X POST \
    -H 'Content-Type: application/json' \
    -d '{"login_name":"tester"}' \
    http://localhost:8081/login

$ curl -X GET \
    http://localhost:8081/users/2
```

## Test

```sh
$ cabal install --only-dependencies --enable-tests
$ cabal configure --enable-tests
$ cabal build
$ cabal test
```

`registerSpec` uses `MockApp`. `MockApp` emulates IO actions and doesn't access to an external process.
