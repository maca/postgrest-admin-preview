Sequel.migration do
  up do
    create_table(Sequel[:api][:todos]) do
      primary_key :id
      String :task, null: false
      DateTime :due
      Bool :done, null: false, default: false
    end

    run "grant select on api.todos to web_anon"
  end

  down do
    drop_table(Sequel[:api][:todos])
  end
end
