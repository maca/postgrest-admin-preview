require 'faker'

DB.extension :pg_enum

Sequel.migration do
  api = Sequel[:api]
  status = %w(idle started finished aproved rejected)

  up do
    DB.create_enum(:todo_status, status)

    create_table(api[:todos]) do
      foreign_key :user_id, api[:users], null: false
      primary_key :id
      varchar :title, null: false
      column :status, :todo_status
      date :due
    end

    run <<-SQL
      grant select on api.todos to todo_anon;
      grant all on api.todos to todo_user;
      grant usage, select on sequence api.todos_id_seq to todo_user;
    SQL

    100.times do
      DB[api[:todos]].insert(
        user_id: DB[api[:users]].order { Sequel.lit('random()') }.first[:id],

        title: Faker::Lorem.sentence,
        status: status.sample,
        due: Date.today + rand(100)
      )
    end
  end

  down do
    # DB.drop_enum(api[:todo_status])
    drop_table(api[:todos])
  end
end
