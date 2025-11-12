-- Permissions for PostgREST API access
-- This file contains all GRANT statements for exposing tables via PostgREST

-- ============================================================================
-- PAGILA TABLES - DVD Rental Store Schema
-- ============================================================================

-- Grant SELECT to web_anon (anonymous/unauthenticated users) on all Pagila tables
GRANT SELECT ON TABLE
  public.actor,
  public.address,
  public.category,
  public.city,
  public.country,
  public.customer,
  public.film,
  public.film_actor,
  public.film_category,
  public.inventory,
  public.language,
  public.payment,
  public.rental,
  public.staff,
  public.store
TO web_anon;

-- Grant full CRUD permissions to web_user (authenticated users) on all Pagila tables
GRANT SELECT, INSERT, UPDATE, DELETE ON TABLE
  public.actor,
  public.address,
  public.category,
  public.city,
  public.country,
  public.customer,
  public.film,
  public.film_actor,
  public.film_category,
  public.inventory,
  public.language,
  public.payment,
  public.rental,
  public.staff,
  public.store
TO web_user;

-- Grant sequence usage for Pagila tables to web_anon
GRANT USAGE, SELECT ON SEQUENCE
  public.actor_actor_id_seq,
  public.address_address_id_seq,
  public.category_category_id_seq,
  public.city_city_id_seq,
  public.country_country_id_seq,
  public.customer_customer_id_seq,
  public.film_film_id_seq,
  public.inventory_inventory_id_seq,
  public.language_language_id_seq,
  public.payment_payment_id_seq,
  public.rental_rental_id_seq,
  public.staff_staff_id_seq,
  public.store_store_id_seq
TO web_anon;

-- Grant sequence usage for Pagila tables to web_user
GRANT USAGE, SELECT ON SEQUENCE
  public.actor_actor_id_seq,
  public.address_address_id_seq,
  public.category_category_id_seq,
  public.city_city_id_seq,
  public.country_country_id_seq,
  public.customer_customer_id_seq,
  public.film_film_id_seq,
  public.inventory_inventory_id_seq,
  public.language_language_id_seq,
  public.payment_payment_id_seq,
  public.rental_rental_id_seq,
  public.staff_staff_id_seq,
  public.store_store_id_seq
TO web_user;


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
