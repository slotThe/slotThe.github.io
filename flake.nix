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
          rev = "5f17ad2b289a67c54767ffa9380db26e0e164c79";
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
                rev = "cfca62e6e95136e48a255e8cbffb0bbe1d98456c";
                hash = "sha256-uATxOdkjWF4oAyke68R5w9A1z1MzhZp8nT/uPERWQlA=";
              };
            }))
          ]))
          deno          # KaTeX rendering of maths—see scripts/math.ts
          zlib
          # Directly rendering TikZ pictures into SVGs
          rubber
          (texlive.combine {
            inherit (texlive) scheme-basic amsmath preview pgf pgfplots tikz-cd;
          })
          poppler_utils
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
