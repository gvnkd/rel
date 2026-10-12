{
  description = "rel — order-book market simulator built with the Redot engine";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs =
    { self, nixpkgs }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];
      forAllSystems = nixpkgs.lib.genAttrs systems;
    in
    {
      packages = forAllSystems (system: {
        redot = nixpkgs.legacyPackages.${system}.callPackage ./nix/redot.nix { };
        default = self.packages.${system}.redot;
      });

      devShells = forAllSystems (system: {
        default = nixpkgs.legacyPackages.${system}.mkShell {
          packages = [
            self.packages.${system}.redot
            nixpkgs.legacyPackages.${system}.gdtoolkit_4
          ];
        };
      });

      checks = forAllSystems (system: {
        engine-tests =
          let
            pkgs = nixpkgs.legacyPackages.${system};
          in
          pkgs.runCommand "rel-engine-tests"
            {
              nativeBuildInputs = [ self.packages.${system}.redot ];
            }
            ''
              cp -r ${pkgs.lib.cleanSource self} proj
              chmod -R u+w proj
              cd proj
              export HOME=$TMPDIR
              redot --headless --path . --import
              redot --headless --path . -s tests.gd
              touch $out
            '';
      });
    };
}
