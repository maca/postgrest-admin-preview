-- Database schema for PostgREST Admin Example
-- This file contains only the database structure (tables, roles, functions)
-- Data is loaded separately from data.sql

-- Enable pgjwt extension for JWT token generation (will also install pgcrypto as dependency)
CREATE EXTENSION IF NOT EXISTS pgjwt CASCADE;

-- Create roles for PostgREST authentication
-- The authenticator role is used by PostgREST to connect to the database
-- It can switch to other roles based on the JWT
DO $$
BEGIN
  IF NOT EXISTS (SELECT FROM pg_roles WHERE rolname = 'authenticator') THEN
    CREATE ROLE authenticator NOINHERIT LOGIN PASSWORD 'mysecretpassword';
  END IF;
END
$$;

-- Create web_anon role if it doesn't exist
-- This is the anonymous/unauthenticated role
DO $$
BEGIN
  IF NOT EXISTS (SELECT FROM pg_roles WHERE rolname = 'web_anon') THEN
    CREATE ROLE web_anon NOLOGIN;
  END IF;
END
$$;

-- Create authenticated user role
-- This role is used for authenticated users
DO $$
BEGIN
  IF NOT EXISTS (SELECT FROM pg_roles WHERE rolname = 'web_user') THEN
    CREATE ROLE web_user NOLOGIN;
  END IF;
END
$$;

-- Grant the authenticator role permission to switch to web_anon and web_user
GRANT web_anon TO authenticator;
GRANT web_user TO authenticator;

-- Create users table for authentication
CREATE TABLE IF NOT EXISTS users (
    id SERIAL PRIMARY KEY,
    email TEXT UNIQUE NOT NULL,
    password TEXT NOT NULL,
    created_at TIMESTAMP DEFAULT NOW()
);

-- Create a simple products table
CREATE TABLE IF NOT EXISTS products (
    id SERIAL PRIMARY KEY,
    name TEXT NOT NULL,
    description TEXT,
    price NUMERIC(10, 2) NOT NULL,
    in_stock BOOLEAN DEFAULT true,
    created_at TIMESTAMP DEFAULT NOW()
);

-- Grant permissions to web_anon role (read-only)
GRANT SELECT ON products TO web_anon;
GRANT USAGE, SELECT ON SEQUENCE products_id_seq TO web_anon;

-- Grant permissions to web_user role (read and write)
GRANT SELECT, INSERT, UPDATE, DELETE ON products TO web_user;
GRANT USAGE, SELECT ON SEQUENCE products_id_seq TO web_user;





-- Login function that validates credentials and returns a JWT
-- Usage: POST to /rpc/login with body: {"email": "user@example.com", "password": "password"}
CREATE OR REPLACE FUNCTION login(email text, password text)
RETURNS json AS $$
DECLARE
  _user users;
  jwt_token text;
BEGIN
  -- Find user by email
  SELECT * INTO _user FROM users WHERE users.email = login.email;

  -- Check if user exists and password is correct
  -- Using SQLSTATE 'PGRST' with proper JSON format for 401 status code
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

  -- Generate JWT token using HS256 with hardcoded secret (must match jwt-secret in postgrest.conf)
  jwt_token := sign(
    json_build_object(
      'role', 'web_user',
      'user_id', _user.id,
      'email', _user.email,
      'exp', extract(epoch from now() + interval '1 hour')::integer
    ),
    'DL+P8+muauKgOSqRKqIKMkjcUpLZ5ajXScgA965i/Bg='
  );

  RETURN json_build_object(
    'token', jwt_token,
    'user', json_build_object(
      'id', _user.id,
      'email', _user.email
    )
  );
END;
$$ LANGUAGE plpgsql SECURITY DEFINER;





-- Grant execute permission to web_anon so unauthenticated users can login
GRANT EXECUTE ON FUNCTION login(text, text) TO web_anon;
