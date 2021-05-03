Sequel.migration do
  up do
    run <<-SQL
      drop role if exists todo_authenticator;
      create role todo_authenticator login;
    SQL
  end

  down do
    run "drop role todo_authenticator"
  end
end
