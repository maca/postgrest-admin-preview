-- Example database schema and data dump
-- This creates a simple table for demonstration purposes

-- Create web_anon role if it doesn't exist
DO $$
BEGIN
  IF NOT EXISTS (SELECT FROM pg_roles WHERE rolname = 'web_anon') THEN
    CREATE ROLE web_anon NOLOGIN;
  END IF;
END
$$;

-- Create a simple products table
CREATE TABLE IF NOT EXISTS products (
    id SERIAL PRIMARY KEY,
    name TEXT NOT NULL,
    description TEXT,
    price NUMERIC(10, 2) NOT NULL,
    in_stock BOOLEAN DEFAULT true,
    created_at TIMESTAMP DEFAULT NOW()
);

-- Grant permissions to web_anon role
GRANT SELECT ON products TO web_anon;
GRANT USAGE, SELECT ON SEQUENCE products_id_seq TO web_anon;

-- Insert sample data
INSERT INTO products (name, description, price, in_stock) VALUES
    ('Widget', 'A useful widget for your projects', 19.99, true),
    ('Gadget', 'An innovative gadget with amazing features', 49.99, true),
    ('Tool', 'Professional-grade tool for experts', 99.99, false),
    ('Device', 'Compact device for everyday use', 29.99, true),
    ('Component', 'Essential component for building systems', 15.99, true);
