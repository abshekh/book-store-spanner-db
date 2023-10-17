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
          haskellProjects.default = { };
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
