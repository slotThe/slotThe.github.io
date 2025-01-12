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
          rev = "ba0301b400cfc5fd9d3e351ea13fcb75c68e571f";
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
            # Compressing fonts.
            p.brotli
            p.fonttools
            p.beautifulsoup4
            # Reasonable syntax highlighting for BQN.
            (p.pygments.overrideAttrs (old: {
              # https://github.com/pygments/pygments/pull/2789
              version = "2.19.a";
              src = fetchFromGitHub {
                owner = "pygments";
                repo = "pygments";
                rev = "43bf86fb86f0a3a4bacedc65eace650947ecee51";
                hash = "sha256-aKxjjKbASA4myEj7zpIxXofwQmpD9Du28fBKo9gmu4I=";
              };
            }))
          ]))
          deno          # KaTeX rendering of maths—see scripts/math.ts
          zlib
        ];
        shellHook = ''
          export PROJECT_ROOT="$(pwd)"
        '';
      };
    };
}
