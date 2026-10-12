{ haskellPackages }:

haskellPackages.callCabal2nix "rel-sim" ../sim { }
