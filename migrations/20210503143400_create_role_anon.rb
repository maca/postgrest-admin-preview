Sequel.migration do
  up do
    run <<-SQL
      drop role if exists app_anon;
      create role app_anon nologin;
    SQL
  end

  down do
    run "drop role app_anon"
  end
end
