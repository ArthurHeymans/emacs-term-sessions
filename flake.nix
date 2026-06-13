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
          meta = {
            description = "Emacs frontend for persistent terminal sessions via zmx";
            homepage = "https://github.com/arthur/term-sessions";
            license = lib.licenses.gpl3Plus;
          };
        };

        checks.default = pkgs.runCommand "term-sessions-check" { nativeBuildInputs = [ pkgs.emacs ]; } ''
          cp ${testSrc}/*.el .
          emacs --batch -Q -L . -f batch-byte-compile $(find . -maxdepth 1 -name '*.el' ! -name 'term-sessions-tests.el' -print | sort)
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
