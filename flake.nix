{ inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/23.05";

    utils.url = github:numtide/flake-utils;

    all-cabal-hashes = {
      url = "https://api.github.com/repos/commercialhaskell/all-cabal-hashes/tarball/c314b1bfabd37d30d2f315a5a322323ebbd98acb";
      type = "file";
      flake = false;
    };
  };

  outputs = { all-cabal-hashes, nixpkgs, utils, ... }:
    utils.lib.eachDefaultSystem (system:
    utils.lib.eachSystem [ "ghc96" ] (compiler:
      let
        config = { };

        overlay = self: super: {
          inherit all-cabal-hashes;

          haskell = super.haskell // {
            packages = super.haskell.packages // {
              "${compiler}" = super.haskell.packages."${compiler}".override (old: {
                overrides =
                  self.lib.fold
                    self.lib.composeExtensions
                    (_: _: { })
                    [ (self.haskell.lib.packageSourceOverrides {
                        hs-opentelemetry-api = "0.1.0.0";

                        hs-opentelemetry-exporter-otlp = "0.0.1.5";

                        hs-opentelemetry-propagator-b3 = "0.0.1.1";

                        hs-opentelemetry-propagator-w3c = "0.0.1.3";

                        hs-opentelemetry-sdk = "0.0.3.6";

                        opentelemetry-plugin =
                          self.lib.cleanSourceWith
                            { filter = name: type:
                                    self.lib.cleanSourceFilter name type
                                &&  !(self.lib.hasSuffix ".nix" name);

                              src = ./.;
                            };
                      })
                    ];
              });
            };
          };
        };

        pkgs =
          import nixpkgs { inherit config system; overlays = [ overlay ]; };

      in
        rec {
          packages = pkgs.haskell.packages."${compiler}".opentelemetry-plugin;

          devShells =
            pkgs.haskell.packages."${compiler}".opentelemetry-plugin.env;
        }
    ));
}
