-- Permissions for PostgREST API access
-- This file contains all GRANT statements for exposing tables via PostgREST

-- ============================================================================
-- BLUEBOX SCHEMA - Grant usage and access
-- ============================================================================

-- Grant USAGE on bluebox schema to web_anon and bluebox roles
GRANT USAGE ON SCHEMA bluebox TO web_anon;
GRANT USAGE ON SCHEMA bluebox TO bluebox;

-- Grant SELECT to web_anon (anonymous/unauthenticated users) on all tables in bluebox schema
GRANT SELECT ON ALL TABLES IN SCHEMA bluebox TO web_anon;

-- Grant full CRUD permissions to bluebox role (authenticated users) on all tables in bluebox schema
GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA bluebox TO bluebox;

-- Grant sequence usage for bluebox schema to web_anon
GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA bluebox TO web_anon;

-- Grant sequence usage for bluebox schema to bluebox role
GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA bluebox TO bluebox;

-- Set default privileges for future tables in bluebox schema
ALTER DEFAULT PRIVILEGES IN SCHEMA bluebox GRANT SELECT ON TABLES TO web_anon;
ALTER DEFAULT PRIVILEGES IN SCHEMA bluebox GRANT SELECT, INSERT, UPDATE, DELETE ON TABLES TO bluebox;
ALTER DEFAULT PRIVILEGES IN SCHEMA bluebox GRANT USAGE, SELECT ON SEQUENCES TO web_anon;
ALTER DEFAULT PRIVILEGES IN SCHEMA bluebox GRANT USAGE, SELECT ON SEQUENCES TO bluebox;


-- ============================================================================
-- FUNCTIONS - API Endpoints
-- ============================================================================

-- Grant execute permission to web_anon so unauthenticated users can login
GRANT EXECUTE ON FUNCTION login(text, text) TO web_anon;



-- ALTER ROLE authenticator SET statement_timeout TO '10s';
-- ALTER ROLE anonymous SET statement_timeout TO '1s';
-- 
-- https://docs.postgrest.org/en/stable/references/configuration.html#db-tx-end
-- Check multiple primary keys
