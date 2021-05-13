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

    100.times do
      DB[api[:posts]].insert(
        user_id: DB[api[:users]].order { Sequel.lit('random()') }.first[:id],

        title: Faker::Quote.famous_last_words,
        content: Faker::Lorem.paragraph,
        published: [true, false].sample,
        creation_date: Date.today + rand(10)
      )
    end

    run <<-SQL
      grant select on api.posts to todo_anon;
      grant all on api.posts to todo_user;
      grant usage, select on sequence api.posts_id_seq to todo_user;
    SQL
  end

  down do
    drop_table(api[:posts])
  end
end
