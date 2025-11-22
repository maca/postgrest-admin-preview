CREATE ROLE authenticator NOINHERIT LOGIN;
CREATE ROLE web_anon NOLOGIN;
CREATE ROLE bluebox NOLOGIN;

GRANT web_anon TO authenticator;
GRANT bluebox TO authenticator;


