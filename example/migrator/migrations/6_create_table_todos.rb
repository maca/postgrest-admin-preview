require 'faker'

DB.extension :pg_enum

Sequel.migration do
  api = Sequel[:api]
  status = %w(idle started finished aproved rejected)

  up do
    DB.create_enum(:app_status, status)

    create_table(api[:todos]) do
      foreign_key :user_id, api[:users], null: false
      primary_key :id
      varchar :title, null: false
      column :status, :app_status

      date :due
    end

    run <<-SQL
      create or replace function api.todos_users_name(api.todos) returns text as $$
        select users.name from api.todos join api.users on api.users.id = $1.user_id limit 1;
      $$ immutable language sql;

      create index todos_users_name_idx on api.todos
        using gin (to_tsvector('english', api.todos_users_name(todos)));
    SQL

    run <<-SQL
      grant select on api.todos to app_anon;
      grant all on api.todos to app_user;
      grant usage, select on sequence api.todos_id_seq to app_user;
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
    # DB.drop_enum(api[:app_status])
    drop_table(api[:todos])
  end
end
