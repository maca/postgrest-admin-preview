require 'faker'

Sequel.migration do
  api = Sequel[:api]

  up do
    create_table(api[:users]) do
      primary_key :id
      varchar :name, null: false, index: true
      column :email, :email, null: false, unique: true
    end

    100.times do
      name = Faker::BossaNova.artist
      email = "#{name.downcase.gsub(' ', '_')}@#{Faker::Internet.domain_name}"

      DB[api[:users]].insert(name: name, email: email)
    end

    run <<-SQL
      grant select on api.users to app_anon;
      grant all on api.users to app_user;
      grant usage, select on sequence api.users_id_seq to app_user;
    SQL
  end

  down do
    drop_table(api[:users])
  end
end
