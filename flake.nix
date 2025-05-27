{ inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/25.05";

    utils.url = github:numtide/flake-utils;

    all-cabal-hashes = {
      url = "https://api.github.com/repos/commercialhaskell/all-cabal-hashes/tarball/5fe87bd08298ee4e481f14cd5096f1f1fcf1394a";
      type = "file";
      flake = false;
    };
  };

  outputs = { all-cabal-hashes, nixpkgs, utils, ... }:
    utils.lib.eachDefaultSystem (system:
    utils.lib.eachSystem [ "ghc910" "ghc912" ] (compiler:
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
                        hs-opentelemetry-api = "0.2.0.0";

                        hs-opentelemetry-exporter-otlp = "0.1.0.0";

                        hs-opentelemetry-propagator-b3 = "0.0.1.2";

                        hs-opentelemetry-propagator-w3c = "0.0.1.4";

                        hs-opentelemetry-sdk = "0.1.0.0";

                        proto-lens = "0.7.1.6";

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
