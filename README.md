# Web App. Testing experimentation

## Prepare

* Redis
* MySQL
* MailCatcher

```sql
CREATE DATABASE test DEFAULT CHARACTER SET utf8mb4;
CREATE TABLE test.user (
    id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(64) NOT NULL,
    email_address VARCHAR(128) NOT NULL,
    created_at DATETIME NOT NULL,
    last_loggedin_at DATETIME NOT NULL,
    UNIQUE KEY unique_user_name (name),
    UNIQUE KEY unique_user_email_address (email_address)
);
```

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

TODO
