{
  description = "PostgREST Admin - Admin interface for PostgREST";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    mkElmDerivation.url = "github:jeslie0/mkElmDerivation";
  };

  outputs = { self, nixpkgs, mkElmDerivation }:
    {
      # Export the deployment module from example/
      nixosModules.default = { config, lib, pkgs, ... }:
        {
          imports = [ ./example/nix/deploy.nix ];

          # Apply mkElmDerivation overlay
          nixpkgs.overlays = [ mkElmDerivation.overlays.default ];
        };
    };
}
