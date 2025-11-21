CREATE EXTENSION IF NOT EXISTS pgjwt CASCADE;
CREATE EXTENSION IF NOT EXISTS postgis CASCADE;

CREATE ROLE authenticator NOINHERIT LOGIN;
CREATE ROLE web_anon NOLOGIN;
CREATE ROLE bluebox NOLOGIN;


GRANT web_anon TO authenticator;
GRANT bluebox TO authenticator;


GRANT USAGE ON SCHEMA bluebox TO web_anon;
GRANT USAGE ON SCHEMA bluebox TO bluebox;


GRANT SELECT ON ALL TABLES IN SCHEMA bluebox TO web_anon;
GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA bluebox TO bluebox;
GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA bluebox TO web_anon;
GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA bluebox TO bluebox;


ALTER DEFAULT PRIVILEGES IN SCHEMA bluebox GRANT SELECT ON TABLES TO web_anon;
ALTER DEFAULT PRIVILEGES IN SCHEMA bluebox GRANT SELECT, INSERT, UPDATE, DELETE ON TABLES TO bluebox;
ALTER DEFAULT PRIVILEGES IN SCHEMA bluebox GRANT USAGE, SELECT ON SEQUENCES TO web_anon;
ALTER DEFAULT PRIVILEGES IN SCHEMA bluebox GRANT USAGE, SELECT ON SEQUENCES TO bluebox;



CREATE TABLE IF NOT EXISTS users (
    id SERIAL PRIMARY KEY,
    email TEXT UNIQUE NOT NULL,
    password TEXT NOT NULL,
    created_at TIMESTAMP DEFAULT NOW()
);



CREATE OR REPLACE FUNCTION login(email text, password text)
RETURNS json AS $$
DECLARE
  _user users;
  _schema_name text;
  jwt_token text;
BEGIN
  SELECT * INTO _user FROM users WHERE users.email = login.email;

  IF _user.id IS NULL THEN
    RAISE SQLSTATE 'PGRST' USING
      message = '{"code": "PGRST401", "message": "Invalid email or password", "details": "Authentication failed", "hint": "Please check your credentials"}',
      detail = '{"status": 401, "headers": {}}';
  END IF;

  IF _user.password != crypt(login.password, _user.password) THEN
    RAISE SQLSTATE 'PGRST' USING
      message = '{"code": "PGRST401", "message": "Invalid email or password", "details": "Authentication failed", "hint": "Please check your credentials"}',
      detail = '{"status": 401, "headers": {}}';
  END IF;

  _schema_name := split_part(_user.email, '@', 1);

  jwt_token := sign(
    json_build_object(
      'role', 'bluebox',
      'user_id', _user.id,
      'email', _user.email,
      'schema', _schema_name,
      'exp', extract(epoch from now() + interval '4 hours')::integer
    ),
    'DL+P8+muauKgOSqRKqIKMkjcUpLZ5ajXScgA965i/Bg='
  );

  RETURN json_build_object(
    'token', jwt_token,
    'user', json_build_object(
      'id', _user.id,
      'email', _user.email,
      'schema', _schema_name
    )
  );
END;
$$ LANGUAGE plpgsql SECURITY DEFINER;
GRANT EXECUTE ON FUNCTION login(text, text) TO web_anon;



INSERT INTO users (email, password) VALUES ('bluebox@example.com', crypt('password', gen_salt('bf')))
ON CONFLICT (email) DO NOTHING;
