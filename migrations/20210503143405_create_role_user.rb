Sequel.migration do
  up do
    run <<-SQL
      drop role if exists todo_user;
      create role todo_user nologin;
      grant todo_user to todo_authenticator;
    SQL
  end

  down do
    run "drop role todo_user"
  end
end
