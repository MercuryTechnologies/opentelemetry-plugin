{ inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/23.05";

    utils.url = github:numtide/flake-utils;
  };

  outputs = { nixpkgs, utils, ... }:
    utils.lib.eachDefaultSystem (system:
    utils.lib.eachSystem [ "ghc96" ] (compiler:
      let
        config = { };

        overlay = self: super: {
          haskell = super.haskell // {
            packages = super.haskell.packages // {
              "${compiler}" = super.haskell.packages."${compiler}".override (old: {
                overrides =
                  self.lib.fold
                    self.lib.composeExtensions
                    (_: _: { })
                    [ (self.haskell.lib.packageSourceOverrides {
                        opentelemetry-plugin =
                          self.lib.cleanSourceWith
                            { filter = name: type:
                                    self.lib.cleanSourceFilter name type
                                &&  !(self.lib.hasSuffix ".nix" name)
                                &&  !(builtins.baseNameOf name == "README.md");

                              src = ./.;
                            };
                      })

                      (self.haskell.lib.packagesFromDirectory {
                        directory = {
                          ghc96 = ./ghc96;
                        }."${compiler}";
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
