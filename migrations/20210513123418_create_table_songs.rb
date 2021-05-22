require 'faker'

DB.extension :pg_array

Sequel.migration do
  api = Sequel[:api]

  up do
    create_table(api[:songs]) do
      foreign_key :user_id, api[:users], null: false
      primary_key :id

      varchar :title, null: false
      column :categories, 'varchar[]'

      date :released
    end

    run <<-SQL
      create or replace function api.songs_users_name(api.songs) returns text as $$
        select users.name from api.songs join api.users on api.users.id = $1.user_id limit 1;
      $$ immutable language sql;

      create index songs_users_name_idx on api.songs
        using gin (to_tsvector('english', api.songs_users_name(songs)));
    SQL

    run <<-SQL
      grant select on api.songs to todo_anon;
      grant all on api.songs to todo_user;
      grant usage, select on sequence api.songs_id_seq to todo_user;
    SQL

    100.times do
      DB[api[:songs]].insert(
        user_id: DB[api[:users]].order { Sequel.lit('random()') }.first[:id],

        title: Faker::BossaNova.song,
        categories: Sequel.pg_array((0..rand(4)).map { Faker::Adjective.positive }),
        released: Faker::Date.between(from: '1965-09-23', to: '2000-09-25')
      )
    end
  end

  down do
    drop_table(api[:songs])
  end
end
