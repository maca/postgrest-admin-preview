-- Sample data for PostgREST Admin Example
-- This file contains only the sample data (can be reloaded independently)

-- Insert test user (password: "password")
INSERT INTO users (email, password) VALUES
    ('user@example.com', crypt('password', gen_salt('bf')))
ON CONFLICT (email) DO NOTHING;
