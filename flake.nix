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
        godot-cpp = nixpkgs.legacyPackages.${system}.callPackage ./nix/godot-cpp.nix { };
        sim = nixpkgs.legacyPackages.${system}.callPackage ./nix/sim.nix { };
        sim-extension =
          nixpkgs.legacyPackages.${system}.callPackage ./nix/gdextension.nix
            {
              godot-cpp = self.packages.${system}.godot-cpp;
              sim = self.packages.${system}.sim;
            };
        default = self.packages.${system}.redot;
      });

      apps = forAllSystems (system: {
        link-sim-extension = {
          type = "app";
          program = toString (
            nixpkgs.legacyPackages.${system}.writeShellScript "link-sim-extension" ''
              set -e
              cd "$(${nixpkgs.legacyPackages.${system}.git}/bin/git rev-parse --show-toplevel)"
              mkdir -p addons/sim_native/bin
              rm -f addons/sim_native/sim_native.gdextension addons/sim_native/bin/*.so
              cp ${self.packages.${system}.sim-extension}/share/sim_native/sim_native.gdextension addons/sim_native/
              cp ${self.packages.${system}.sim-extension}/share/sim_native/bin/*.so addons/sim_native/bin/
              chmod u+w addons/sim_native/sim_native.gdextension addons/sim_native/bin/*.so
              echo "linked sim_native extension into addons/sim_native/"
            ''
          );
        };
      });

      devShells = forAllSystems (system: {
        default = nixpkgs.legacyPackages.${system}.mkShell {
          packages = [
            self.packages.${system}.redot
            self.packages.${system}.sim
            nixpkgs.legacyPackages.${system}.gdtoolkit_4
            (nixpkgs.legacyPackages.${system}.haskellPackages.ghcWithPackages (
              ps: with ps; [ QuickCheck splitmix ]
            ))
          ];
        };
      });

      checks = forAllSystems (system: {
        sim-generate =
          let
            pkgs = nixpkgs.legacyPackages.${system};
          in
          pkgs.runCommand "rel-sim-generate"
            {
              nativeBuildInputs = [ self.packages.${system}.sim ];
            }
            ''
              ${self.packages.${system}.sim}/bin/sim --candles 100 --orders-per-candle 50,50 > candles.csv
              grep -q "open,high,low,close" candles.csv
              test $(wc -l < candles.csv) -eq 101
              cp candles.csv $out
            '';
        scene-loads =
          let
            pkgs = nixpkgs.legacyPackages.${system};
          in
          pkgs.runCommand "rel-scene-loads"
            {
              nativeBuildInputs = [ self.packages.${system}.redot ];
            }
            ''
              cp -r ${pkgs.lib.cleanSource self} proj
              chmod -R u+w proj
              cd proj
              export HOME=$TMPDIR
              redot --headless --path . --import
              redot --headless --path . --quit-after 2 chart.tscn
              touch $out
            '';
      });
    };
}
