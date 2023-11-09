{
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixpkgs-unstable;
    utils.url   = github:numtide/flake-utils;
  };

  outputs = { nixpkgs, utils, ... }:
    utils.lib.eachDefaultSystem (system:
      let
        overlay = final: prev: {
          haskellPackages = prev.haskellPackages.override (old: {
            overrides = final.haskell.lib.packageSourceOverrides {
              site = ./.;
            };
          });
        };
        pkgs = import nixpkgs { inherit system; overlays = [ overlay ]; };
      in {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            (python311.withPackages (p: [ p.pygments ])) # Syntax highlighting
            nodejs_20  # Offline LaTeX SVG creation
            parallel   # Process LaTeX snippets in parallel
          ];
          inputsFrom  = [ pkgs.haskellPackages.site.env ];
        };
        }
    );
}
