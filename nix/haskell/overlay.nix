nixpkgsSelf: nixpkgsSuper:

let
  inherit (nixpkgsSelf) pkgs;

  ghcVersion = "ghc865";

  hsPkgs = nixpkgsSuper.haskell.packages.${ghcVersion}.override {
    overrides = self: super: {
    };
  };

in
{
  haskell = nixpkgsSuper.haskell // {
    inherit ghcVersion;

    packages = nixpkgsSuper.haskell.packages // {
      statistics =
          let st = pkgs.haskell.packages.${ghcVersion}.statistics;
          in pkgs.haskell.lib.dontCheck st;
      "${ghcVersion}" = hsPkgs;
    };
  };
}
