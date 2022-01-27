require 'faker'

Sequel.migration do
  schema = Sequel[:basic_auth]

  up do
    run <<-SQL
      create schema if not exists basic_auth;
      create extension if not exists pgcrypto;
      create extension if not exists pgjwt;
      create extension if not exists citext;
      create domain email
              as citext check
              ( value ~ '^[a-zA-Z0-9.!#$%&''*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$' );
    SQL

    create_table(schema[:admins]) do
      primary_key :id

      varchar :name, null: false, index: true

      column :email, :email, null: false, unique: true
      varchar :role, null: false, index: true
      varchar :password, null: false
    end

    # This shouldn't be here
    run <<-SQL
      ALTER DATABASE "postgres_db"
            SET "app.jwt_secret"
            TO '6oKDJMClxO1/6GiWRIvfYocH/w4yPerCHyV0BmAQius=';
    SQL

    # Set constraint on role to allow only existing roles
    run <<-SQL
      create or replace function
      basic_auth.check_role_exists() returns trigger as $$
      begin
        if not exists
          (select 1 from pg_roles as r where r.rolname = new.role) then

          raise foreign_key_violation using message =
            'unknown database role: ' || new.role;
          return null;
        end if;
        return new;
      end
      $$ language plpgsql;

      drop trigger if exists ensure_user_role_exists on basic_auth.admins;
      create constraint trigger ensure_user_role_exists
        after insert or update on basic_auth.admins
        for each row
        execute procedure basic_auth.check_role_exists();
    SQL


    # Encrypt password on insert and update
    run <<-SQL
      create or replace function
      basic_auth.encrypt_password() returns trigger as $$
      begin
        if tg_op = 'INSERT' or new.password <> old.password then
          new.password = crypt(new.password, gen_salt('bf'));
        end if;
        return new;
      end
      $$ language plpgsql;

      drop trigger if exists encrypt_password on basic_auth.admins;
      create trigger encrypt_password
        before insert or update on basic_auth.admins
        for each row
        execute procedure basic_auth.encrypt_password();
    SQL


    DB[schema[:admins]].insert(
      name: 'Admin',
      email: 'admin@example.com',
      role: 'app_user',
      password: '123456'
    )


    run <<-SQL
      CREATE TYPE basic_auth.jwt_token AS (token text);

      create or replace function
      basic_auth.user_role(email text, password text) returns name
        language plpgsql
        as $$
      begin
        return (
        select role from basic_auth.admins
        where admins.email = user_role.email
          and admins.password = crypt(user_role.password, admins.password)
        );
      end;
      $$;

      create or replace function
      api.login(email text, password text) returns basic_auth.jwt_token as $$
      declare
        _role name;
        result basic_auth.jwt_token;
      begin
        select basic_auth.user_role(email, password) into _role;

        if _role is null then
          raise invalid_password using message = 'invalid user or password';
        end if;

        select public.sign(row_to_json(r), current_setting('app.jwt_secret'))
          as token
          from (
            select _role as role, login.email as email,
              extract(epoch from now())::integer + 60*60 as exp
          ) r
          into result;
        return result;
      end;
      $$ language plpgsql security definer;

      grant execute on function api.login(text,text) to app_anon;
    SQL
  end

  down do
    run <<-SQL
      DROP FUNCTION api.login;
      DROP SCHEMA basic_auth CASCADE;
    SQL
  end
end
