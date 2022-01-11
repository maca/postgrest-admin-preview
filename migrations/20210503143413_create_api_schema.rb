Sequel.migration do
  up do
    run <<-SQL
      create schema api;
      grant usage on schema api to app_anon;
      grant usage on schema api to app_user;
    SQL
  end

  down do
    run "drop schema api"
  end
end
