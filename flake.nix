{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";
  inputs.haskell-flake.url = "github:srid/haskell-flake";
  outputs =
    inputs@{ nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
      ];
      perSystem = { self', system, pkgs, devShells, ... }:
        {
          _module.args.pkgs = import inputs.nixpkgs { inherit system; config.allowBroken = true; };
          haskellProjects.default = {
            basePackages = pkgs.haskell.packages.ghc8107;
            packages = {
              aeson.source = "1.5.6.0";
              hashable.source = "1.3.0.0";
              hashable-time.source = "0.2.1";
              OneTuple.source = "0.2.2.1";
              base-compat-batteries.source = "0.11.2";
              base-compat.source = "0.11.2";
              time-compat.source = "1.9.5";
              http2.source = "3.0.2";
              quickcheck-instances.source = "0.3.25.2";
            };
            settings = {
              hashable.jailbreak = true;
              doctest.check = false;
              hspec-megaparsec.check = false;

            };
          };
          packages.default = self'.packages.book-store-spanner-db;
        };
    };
}
