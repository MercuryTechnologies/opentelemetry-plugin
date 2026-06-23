{ inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/26.05";
    
    utils.url = "github:numtide/flake-utils";

    all-cabal-hashes = {
      url = "https://api.github.com/repos/commercialhaskell/all-cabal-hashes/tarball/1b1b8b07a6b0da936c906de2c2cb77a4c407c88f";
      type = "file";
      flake = false;
    };
  };

  outputs = { all-cabal-hashes, nixpkgs, utils, ... }:
    utils.lib.eachDefaultSystem (system:
    utils.lib.eachSystem [ "ghc96" "ghc910" ] (compiler:
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
                        hs-opentelemetry-api = "0.3.1.0";

                        hs-opentelemetry-exporter-otlp = "0.1.1.0";

                        hs-opentelemetry-propagator-b3 = "0.0.1.3";

                        hs-opentelemetry-propagator-w3c = "0.1.0.0";

                        hs-opentelemetry-sdk = "0.1.0.1";

                        thread-utils-context = "0.3.0.4";
                        
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
