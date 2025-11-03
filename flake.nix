{
  inputs = {
    opam-nix.url = "github:tweag/opam-nix";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.follows = "opam-nix/nixpkgs";

  };
  outputs = { self, flake-utils, opam-nix, nixpkgs, }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        on = opam-nix.lib.${system};
        localPackagesQuery = builtins.mapAttrs (_: pkgs.lib.last)
          (on.listRepo (on.makeOpamRepo ./.));
        devPackagesQuery = {
          # You can add "development" packages here. They will get added to the devShell automatically.
          ocaml-lsp-server = "*";
          ocamlformat = "*";
        };
        query = devPackagesQuery // { ocaml-base-compiler = "5.3.0"; };
        scope =
          on.buildOpamProject' { resolveArgs.with-test = true; } ./. query;
        overlay = final: prev:
          {
            # You can add overrides here
            pbrt = prev.pbrt.overrideAttrs (oldAttrs: {
              src = pkgs.fetchgit {
                url = "https://github.com/mransan/ocaml-protoc.git";
                rev = "ad297983ff7f253f309ed77f38c2a93d0d01bfdf";
                sha256 = "sha256-KPctDwJIGi2W3HJEZBPwu9qRUrqK0azfW7VKCPxzCOU=";
              };
            });
          };
        scope' = scope.overrideScope overlay;
        # Packages from devPackagesQuery
        devPackages = builtins.attrValues
          (pkgs.lib.getAttrs (builtins.attrNames devPackagesQuery) scope');
        # Packages in this workspace
        packages =
          pkgs.lib.getAttrs (builtins.attrNames localPackagesQuery) scope';
      in {
        legacyPackages = scope';

        inherit packages;

        ## If you want to have a "default" package which will be built with just `nix build`, do this instead of `inherit packages;`:
        # packages = packages // { default = packages.<your default package>; };

        devShells.default = pkgs.mkShell {
          inputsFrom = builtins.attrValues packages;
          buildInputs = devPackages ++ [
            # You can add packages from nixpkgs here
          ];
        };
      });
}
