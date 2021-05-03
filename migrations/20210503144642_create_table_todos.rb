Sequel.migration do
  up do
    create_table(Sequel[:api][:todos]) do
      primary_key :id
      String :task, null: false
      DateTime :due
      Bool :done, null: false, default: false
    end

    run <<-SQL
      grant select on api.todos to todo_anon;
      grant all on api.todos to todo_user;
      grant usage, select on sequence api.todos_id_seq to todo_user;
    SQL
  end

  down do
    drop_table(Sequel[:api][:todos])
  end
end
