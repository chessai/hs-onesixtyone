with rec {
  config = {
    allowBroken = true;
  };

  overlays = [
    (import ./overlay.nix)
  ];

  pkgs = import <nixpkgs> { inherit config overlays; };
};

pkgs.haskell.packages.ghc881.shellFor {
  packages = ps: with ps; [ onesixtyone ];
}
