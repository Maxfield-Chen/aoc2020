{pkgs ? import ../nixpkgs {} }:

let
  inherit (pkgs.haskell) ghcVersion;

  hsPkgs = pkgs.haskell.packages.${ghcVersion};

  pkgDrv = hsPkgs.callCabal2nix "aoc2020" ../.. {};
  haskellDeps = pkgDrv.getBuildInputs.haskellBuildInputs;
  ghc = hsPkgs.ghcWithHoogle (_: haskellDeps);

in
{
  inherit ghc;
  inherit (hsPkgs) cabal-install hlint ghcide ghcid stylish-haskell ormolu hpack;
}
