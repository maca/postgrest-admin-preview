Sequel.migration do
  up do
    run <<-SQL
      drop role if exists app_user;
      create role app_user nologin;
    SQL
  end

  down do
    run "drop role app_user"
  end
end
