{
  description = "PostgREST Admin - Admin interface for PostgREST";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    {
      # Export the deployment module from example/
      nixosModules.default = import ./example/nix/deploy.nix;
    };
}
