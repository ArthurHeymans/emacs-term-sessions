{
  description = "Emacs frontend for persistent terminal sessions via zmx";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    zmx.url = "github:neurosnap/zmx";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      zmx,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        inherit (pkgs) lib;
        zmxPkg = zmx.packages.${system}.default;
        packageSrc = lib.cleanSourceWith {
          src = ./.;
          filter =
            path: type:
            let
              base = baseNameOf path;
            in
            base != "term-sessions-tests.el" && !lib.hasSuffix ".elc" base && base != "progress.md";
        };
        testSrc = lib.cleanSourceWith {
          src = ./.;
          filter =
            path: type:
            let
              base = baseNameOf path;
            in
            !lib.hasSuffix ".elc" base && base != "progress.md";
        };
      in
      {
        packages.default = pkgs.emacsPackages.trivialBuild {
          pname = "term-sessions";
          version = "0.1.0";
          src = packageSrc;
          packageRequires = with pkgs.emacsPackages; [ ];
        };

        checks.default = pkgs.runCommand "term-sessions-check" { nativeBuildInputs = [ pkgs.emacs ]; } ''
          cp ${testSrc}/term-sessions.el .
          cp ${testSrc}/term-sessions-tests.el .
          emacs --batch -Q -L . -f batch-byte-compile term-sessions.el
          emacs --batch -Q -L . -l term-sessions-tests.el -f ert-run-tests-batch-and-exit
          touch $out
        '';

        devShells.default = pkgs.mkShell {
          packages = [
            zmxPkg
            pkgs.emacs
          ];
        };
      }
    );
}
