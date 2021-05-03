Sequel.migration do
  up do
    run <<-SQL
      drop role if exists web_anon;
      create role web_anon nologin;
      grant usage on schema api to web_anon;
    SQL
  end

  down do
    run "drop role web_anon"
  end
end
