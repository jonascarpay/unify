{
  description = "unify";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-22.11";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = inputs:
    let
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = hfinal: hprev:
            prev.haskell.packageOverrides hfinal hprev // {
              unify = hfinal.callCabal2nix "unify" ./. { };
            };
        };
        unify = final.haskell.lib.compose.justStaticExecutables final.haskellPackages.unify;
      };
      perSystem = system:
        let
          pkgs = import inputs.nixpkgs { inherit system; overlays = [ overlay ]; };
          hspkgs = pkgs.haskellPackages;
        in
        {
          devShell = hspkgs.shellFor {
            withHoogle = true;
            packages = p: [ p.unify ];
            buildInputs = [
              hspkgs.cabal-install
              hspkgs.haskell-language-server
              hspkgs.hlint
              hspkgs.ormolu
              pkgs.bashInteractive
            ];
          };
          defaultPackage = pkgs.unify;
        };
    in
    { inherit overlay; } // inputs.flake-utils.lib.eachDefaultSystem perSystem;
}
