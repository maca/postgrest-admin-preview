Sequel.migration do
  up do
    run "create schema api"
  end

  down do
    run "drop schema api"
  end
end
