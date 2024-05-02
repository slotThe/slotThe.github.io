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
            url = "https://github.com/slotThe/pandoc-sidenote";
            ref = "feat/html-sidenotes";
            rev = "a87380c13dd0433c2614164477f5a1b425329c22";
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
          zlib
          (python3.withPackages (p: [
            p.fonttools # Compressing fonts
            p.brotli    # Compressing fonts
            p.pygments  # Syntax highlighting
          ]))
          nodejs_20  # Offline LaTeX SVG creation
          parallel   # Process LaTeX snippets in parallel
        ];
      };
    };
}
