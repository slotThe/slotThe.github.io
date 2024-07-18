{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { nixpkgs, ... }:
    let
      # Only care about actual Haskell source files; this improves caching
      # behaviour.
      haskellSourceFilter = src: nixpkgs.lib.cleanSourceWith {
        inherit src;
        filter = name: type:
          let file = toString name;
              baseName = baseNameOf file;
          in nixpkgs.lib.cleanSourceFilter name type
             && !(nixpkgs.lib.hasPrefix "_" baseName
                  || nixpkgs.lib.hasPrefix "." baseName
                  || nixpkgs.lib.hasSuffix ".md" file
                  || nixpkgs.lib.hasSuffix ".css" file
                  || nixpkgs.lib.hasSuffix ".html" file
                  || baseName == "bib"
                  || baseName == "css"
                  || baseName == "dist-newstyle"
                  || baseName == "docs"
                  || baseName == "fonts"
                  || baseName == "images"
                  || baseName == "mathjax-node-page"
                  || baseName == "posts"
                  || baseName == "talks"
                  || baseName == "templates");
      };
      system = "x86_64-linux";
      pkgs   = nixpkgs.legacyPackages.${system};
      hPkgs  = pkgs.haskellPackages.extend (self: super: {
        site            = self.callCabal2nix "site" (haskellSourceFilter ./.) { };
        pandoc-sidenote = self.callCabal2nixWithOptions "pandoc-sidenote"
          (builtins.fetchGit {
            url = "https://github.com/jez/pandoc-sidenote";
            rev = "3658e7da9453fb6ab817d8eef5d1928cbcd3afbf";
          })
          "-f html-sidenotes"
          { };
      });
    in {
      # nix build
      packages.${system}.default = hPkgs.site;

      # nix develop
      devShells.${system}.default = hPkgs.shellFor {
        packages          = p: [ p.site ];
        nativeBuildInputs = [ hPkgs.haskell-language-server ];
        buildInputs       = with pkgs; [
          deno       # KaTeX rendering of maths—see scripts/math.ts
          (python3.withPackages (p: [
            p.fonttools # Compressing fonts
            p.brotli    # Compressing fonts
            p.pygments  # Syntax highlighting
          ]))
          zlib
        ];
      };
    };
}
