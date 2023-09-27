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
              base-compat-batteries.source = "0.11.2";
              base-compat.source = "0.11.2";
              hashable.source = "1.4.0.0";
              http2.source = "3.0.2";
              indexed-traversable-instances.source = "0.1";
              lens-aeson.source = "1.1.3";
            };
            settings = {
              aeson.jailbreak = true;
              doctest.check = false;
              hashable.jailbreak = true;
              hspec-megaparsec.check = false;
              lsp-types.jailbreak = true;
              rebase.jailbreak = true;
              tree-diff.jailbreak = true;
            };
          };
          packages.default = self'.packages.book-store-spanner-db;
        };
    };
}
