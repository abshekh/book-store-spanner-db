{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";
  inputs.haskell-flake.url = "github:srid/haskell-flake";
  outputs =
    inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
      ];
      perSystem = { self', system, pkgs, lib, ... }:
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
          packages.book-store-spanner-db-docker = pkgs.symlinkJoin {
            name = "book-store-spanner-db";
            paths = pkgs.haskell.lib.justStaticExecutables self'.packages.book-store-spanner-db;
            postBuild = ''
              mkdir -p $out/opt/app/
              for f in `${lib.getExe pkgs.fd} . $out/bin/`; do
                ln -s $f $out/opt/app/
              done
            '';
          };
          packages.dockerImage =
            pkgs.dockerTools.buildImage {
              name = "book-store-spanner-db";
              created = "now";
              tag = builtins.substring 0 9 (self.rev or "dev");
              copyToRoot = pkgs.buildEnv {
                paths = with pkgs; [
                  bash
                  gnused
                  gzip
                  curl
                  inetutils
                  dnsutils
                  netcat
                  libxml2
                  postgresql_12
                  coreutils

                  self'.packages.book-store-spanner-db-docker
                ];
                name = "book-store-spanner-db-root";
                pathsToLink = [
                  "/bin"
                  "/opt"
                ];
              };
              config = {
                Env = [
                  "SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
                  # Ref: https://hackage.haskell.org/package/x509-system-1.6.7/docs/src/System.X509.Unix.html#getSystemCertificateStore
                  "SYSTEM_CERTIFICATE_PATH=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
                ];
                Cmd = [ "${self'.packages.book-store-spanner-db-docker}/bin/book-store-spanner-db-exe" ];
              };
            };
        };
    };
}
