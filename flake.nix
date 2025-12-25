{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    git-intro = {
      url = "git+https://gitlab.mn.tu-dresden.de/s0428072/git-introduction?submodules=1";
      flake = false;
    };
  };

  outputs = { nixpkgs, git-intro, ... }:
    let
      # Only care about actual Haskell source files; this improves caching
      # behaviour.
      haskellSourceFilter = src: nixpkgs.lib.cleanSourceWith {
        inherit src;
        filter = name: type:
          let file = toString name;
              baseName = baseNameOf file;
          in nixpkgs.lib.cleanSourceFilter name type
             && (nixpkgs.lib.hasPrefix "src" baseName
                 || nixpkgs.lib.hasSuffix ".cabal" file
                 || nixpkgs.lib.hasSuffix ".project" file
                 || nixpkgs.lib.hasSuffix ".hs" file
                 || nixpkgs.lib.hasSuffix ".nix" file
             );
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
        hakyll = self.callCabal2nix "hakyll" (builtins.fetchGit {
          url = "https://github.com/jaspervdj/hakyll";
          rev = "800a612923fac90017b2050ccde3e031af09091e";
        }) {};
      });
    in {
      # nix build
      packages.${system}.default = hPkgs.site;

      # nix develop
      devShells.${system}.default = hPkgs.shellFor {
        packages          = p: [ p.site ];
        nativeBuildInputs = [ hPkgs.haskell-language-server ];
        buildInputs       = with pkgs; [
          (python3.withPackages (p: [
            # Font compressing
            p.brotli
            p.fonttools
            p.beautifulsoup4
            # BQN syntax highlighting; contains https://github.com/pygments/pygments/pull/2789
            (p.pygments.overrideAttrs (old: {
              version = "2.19.2";
              src = fetchFromGitHub {
                owner = "pygments";
                repo = "pygments";
                rev = "0328cfaf1d953b3a0c7eb0ec0efd363deb2f9d51";
                hash = "sha256-toYPxqaOObK8S4vL58+QippZ53jWW8tKJ2xcL0bHll8=";
              };
            }))
          ]))
          optipng
          zlib
          # KaTeX rendering of maths, see scripts/maths.js
          nodejs
          nodePackages.katex
          # Directly rendering TikZ pictures into SVGs
          rubber
          (texlive.combine {
            inherit (texlive) scheme-basic amsmath preview pgf pgfplots tikz-cd;
          })
          poppler-utils
        ];
        shellHook = ''
          export PROJECT_ROOT="$(pwd)"
          GI="./talks/git-introduction"
          ln -sf ${git-intro}/README.md "$GI.md"
          mkdir -p "$GI"
          ln -sf ${git-intro}/transcript.md "$GI/transcript.md"
        '';
      };
    };
}
