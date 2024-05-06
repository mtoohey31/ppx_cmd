{
  description = "ppx_cmd";

  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils }: {
    overlays.default = _: prev: {
      ocamlPackages = prev.ocamlPackages.overrideScope (final: _: {
        ppx_cmd = final.buildDunePackage {
          pname = "ppx_cmd";
          version = "0.1.0";
          src = builtins.path { path = ./.; name = "ppx_cmd-src"; };
          buildInputs = [
            final.cppo
            final.ounit2
            final.ppxlib
            final.ppx_deriving
          ];
          minimalOcamlVersion = "4.14.1";
          duneVersion = "3";
        };
      });
    };
  } // utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs {
        overlays = [ self.overlays.default ];
        inherit system;
      };
      inherit (pkgs) mkShell ocamlformat ocamlPackages;
      inherit (ocamlPackages) ppx_cmd ocaml-lsp;
    in
    {
      packages.default = ppx_cmd;

      devShells.default = mkShell {
        inputsFrom = [ ppx_cmd ];
        packages = [ ocamlformat ocaml-lsp ];
      };
    });
}
