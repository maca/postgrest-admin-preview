-- Sample data for PostgREST Admin Example
-- This file contains only the sample data (can be reloaded independently)

-- Insert test user (password: "password")
INSERT INTO users (email, password) VALUES
    ('user@example.com', crypt('password', gen_salt('bf')))
ON CONFLICT (email) DO NOTHING;

-- Insert sample products
INSERT INTO products (name, description, price, in_stock) VALUES
    ('Widget', 'A useful widget for your projects', 19.99, true),
    ('Gadget', 'An innovative gadget with amazing features', 49.99, true),
    ('Tool', 'Professional-grade tool for experts', 99.99, false),
    ('Device', 'Compact device for everyday use', 29.99, true),
    ('Component', 'Essential component for building systems', 15.99, true)
ON CONFLICT DO NOTHING;
