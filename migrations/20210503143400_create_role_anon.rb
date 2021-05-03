Sequel.migration do
  up do
    run <<-SQL
      drop role if exists todo_anon;
      create role todo_anon nologin;
    SQL
  end

  down do
    run "drop role todo_anon"
  end
end
