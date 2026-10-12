{ pkgs }:

let
  inherit (pkgs.haskell.lib) overrideCabal;
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      splitmix = overrideCabal super.splitmix (drv: {
        configureFlags = (drv.configureFlags or [ ]) ++ [ "--ghc-options=-fPIC" ];
      });
    };
  };
in
haskellPackages.callCabal2nix "rel-sim" ../sim { }
