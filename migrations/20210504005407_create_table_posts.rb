require 'faker'

Sequel.migration do
  api = Sequel[:api]

  up do
    create_table(api[:posts]) do
      primary_key :id
      foreign_key :user_id, api[:users], null: false

      varchar :title, null: false
      text :content, null: false
      bool :published, null: false, default: false
      timestamp :creation_date, null: false
    end

    run <<-SQL
      create or replace function api.posts_users_name(api.posts) returns text as $$
        select users.name from api.posts join api.users on api.users.id = $1.user_id limit 1;
      $$ immutable language sql;

      create index posts_users_name_idx on api.posts
        using gin (to_tsvector('english', api.posts_users_name(posts)));
    SQL

    run <<-SQL
      grant select on api.posts to todo_anon;
      grant all on api.posts to todo_user;
      grant usage, select on sequence api.posts_id_seq to todo_user;
    SQL

    100.times do
      DB[api[:posts]].insert(
        user_id: DB[api[:users]].order { Sequel.lit('random()') }.first[:id],

        title: Faker::Quote.famous_last_words,
        content: Faker::Lorem.paragraph,
        published: [true, false].sample,
        creation_date: Date.today + rand(10)
      )
    end
  end

  down do
    drop_table(api[:posts])
  end
end
