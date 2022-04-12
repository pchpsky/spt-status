# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: CC0-1.0

{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskellPackages.override {
          # overrides = self: super: rec {
          #   relude = super.relude_1_0_0_1;
          # };
        };

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

        packageName = "spt-status";

        package = haskellPackages.callCabal2nix packageName self rec {
            # Dependency overrides go here
        };
      in {
        packages.${packageName} = package;

        legacyPackages.${packageName} = package;

        defaultPackage = self.packages.${system}.${packageName};

        devShell = pkgs.mkShell {
          buildInputs = with haskellPackages; [
            haskell-language-server
            ghcid
            cabal-install
            pkgs.zlib
            cabal2nix
          ];
          inputsFrom = builtins.attrValues self.packages.${system};

          shellHook = ''
            export LD_LIBRARY_PATH=${pkgs.zlib}/lib
          '';
        };
      });
}
