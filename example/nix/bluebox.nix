{ pkgs }:

{
  # Fetch BlueBox sample database SQL files from GitHub
  schema = pkgs.fetchurl {
    url = "https://raw.githubusercontent.com/ryanbooz/bluebox/main/bluebox_schema_v0.4.sql";
    sha256 = "sha256-1LgfPkaVXG08rlusEclOHKsKaTOzLW8i+vzC0zRnxbM=";
  };

  data = pkgs.fetchurl {
    url = "https://raw.githubusercontent.com/ryanbooz/bluebox/main/bluebox_dataonly_v0.4.sql.zip";
    sha256 = "sha256-TXm47I3UWReyZo1KbhBNTTYMsccAYgnBZcTWfcRnGUE=";
  };
}
