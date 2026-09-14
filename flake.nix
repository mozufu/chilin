{
  description = "Chilin — Git hosting with a myque collaboration store";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/34ab99075ac4f7e40cf037eef32cb1c360bb85e9";
    myque = {
      url = "github:mozufu/myque/b37ce24c6541d7bf3581e890b5df29c782701cd5";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      myque,
    }:
    let
      systems = [
        "aarch64-darwin"
        "x86_64-darwin"
        "aarch64-linux"
        "x86_64-linux"
      ];
      forAllSystems = nixpkgs.lib.genAttrs systems;
      source = nixpkgs.lib.cleanSourceWith {
        src = ./.;
        filter =
          path: type:
          let
            name = baseNameOf path;
          in
          !(builtins.elem name [
            ".git"
            ".direnv"
            "dist-newstyle"
            "result"
            "data"
          ]);
      };
      packagesFor =
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          hs = pkgs.haskellPackages.override {
            overrides = final: prev: {
              myque = final.callPackage "${myque}/myque.nix" {
                src = pkgs.lib.cleanSource myque;
              };
            };
          };
          chilin = hs.callPackage ./chilin.nix {
            src = source;
            inherit (pkgs) git makeWrapper;
          };
          web = pkgs.callPackage ./web { };
        in
        {
          inherit
            pkgs
            hs
            chilin
            web
            ;
        };
    in
    {
      packages = forAllSystems (
        system:
        let
          p = packagesFor system;
        in
        {
          default = p.chilin;
          chilin = p.chilin;
          web = p.web;
        }
      );
      apps = forAllSystems (system: {
        default = {
          type = "app";
          program = "${self.packages.${system}.default}/bin/chilin";
        };
      });
      devShells = forAllSystems (
        system:
        let
          p = packagesFor system;
        in
        {
          default = p.hs.shellFor {
            packages = _: [
              p.chilin
              p.hs.myque
            ];
            nativeBuildInputs = with p.pkgs; [
              cabal-install
              git
              curl
              sqlite
              python3
              pkg-config
              haskell-language-server
              nixfmt
              p.hs.fourmolu
              p.hs.hlint
              p.hs.cabal-fmt
              nodejs_22
              pnpm
            ];
            CHILIN_GIT = "${p.pkgs.git}/bin/git";
          };
        }
      );
      checks = forAllSystems (
        system:
        let
          p = packagesFor system;
        in
        {
          build = p.chilin;
          format =
            p.pkgs.runCommand "chilin-format"
              {
                nativeBuildInputs = [
                  p.hs.fourmolu
                  p.hs.cabal-fmt
                  p.pkgs.nixfmt
                ];
              }
              ''
                cd ${source}
                fourmolu --mode check app/*.hs src/Chilin/*.hs test/*.hs
                cabal-fmt --check chilin.cabal
                nixfmt --check flake.nix chilin.nix
                touch $out
              '';
          lint =
            p.pkgs.runCommand "chilin-lint"
              {
                nativeBuildInputs = [ p.hs.hlint ];
              }
              ''
                cd ${source}
                hlint app src test
                touch $out
              '';
        }
      );
      formatter = forAllSystems (system: nixpkgs.legacyPackages.${system}.nixfmt);
    };
}
