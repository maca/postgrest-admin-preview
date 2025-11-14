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

-- Create authenticated user role for bluebox
-- This role is used for authenticated users
DO $$
BEGIN
  IF NOT EXISTS (SELECT FROM pg_roles WHERE rolname = 'bluebox') THEN
    CREATE ROLE bluebox NOLOGIN;
  END IF;
END
$$;

-- Grant the authenticator role permission to switch to web_anon and bluebox
GRANT web_anon TO authenticator;
GRANT bluebox TO authenticator;

-- Configure search_path for bluebox role to use bluebox schema
-- Users table stays in public schema (default)
ALTER ROLE bluebox SET search_path TO bluebox, public;

-- Create users table for authentication
CREATE TABLE IF NOT EXISTS users (
    id SERIAL PRIMARY KEY,
    email TEXT UNIQUE NOT NULL,
    password TEXT NOT NULL,
    created_at TIMESTAMP DEFAULT NOW()
);

-- Login function that validates credentials and returns a JWT
-- Usage: POST to /rpc/login with body: {"email": "bluebox@example.com", "password": "password"}
-- The schema is determined from the email prefix (e.g., "bluebox" from "bluebox@example.com")
CREATE OR REPLACE FUNCTION login(email text, password text)
RETURNS json AS $$
DECLARE
  _user users;
  _schema_name text;
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

  -- Extract schema name from email (part before @)
  -- e.g., "bluebox" from "bluebox@example.com"
  _schema_name := split_part(_user.email, '@', 1);

  -- Generate JWT token using HS256 with hardcoded secret (must match jwt-secret in postgrest.conf)
  -- Token expires in 30 minutes (1800 seconds)
  jwt_token := sign(
    json_build_object(
      'role', 'bluebox',
      'user_id', _user.id,
      'email', _user.email,
      'schema', _schema_name,
      'exp', extract(epoch from now() + interval '30 minutes')::integer
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
