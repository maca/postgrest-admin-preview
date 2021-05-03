Sequel.migration do
  up do
    run <<-SQL
      create schema api;
      grant usage on schema api to todo_anon;
      grant usage on schema api to todo_user;
    SQL
  end

  down do
    run "drop schema api"
  end
end
