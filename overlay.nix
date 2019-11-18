self: super:

with rec {
  inherit (super) lib fetchFromGitHub;

  hlib = super.haskell.lib;

  mainOverlay = hself: hsuper: {
    onesixtyone = hself.callCabal2nix "onesixtyone" ./. {};

    language-asn = hlib.doJailbreak hsuper.language-asn;
    snmp = hlib.doJailbreak (hself.callCabal2nix "snmp" (fetchFromGitHub {
      owner = "haskell-snmp";
      repo = "snmp";
      rev = "9a112f16ee242cd65ded506f98591bc40970f5f1";
      sha256 = "1r4sby4j0x6hp4b724w6vrg9ma215bz1jqciljbijr6jmqwjk5ms";
    }) {});
    semirings = hsuper.semirings_0_5_2;
    quickcheck-classes = hsuper.quickcheck-classes_0_6_4_0;
    wide-word = hself.callCabal2nix "wide-word" (fetchFromGitHub {
      owner = "erikd";
      repo = "wide-word";
      rev = "c3381c7d9c04a6f4e87cc383bb478101a2008cf0";
      sha256 = "0hdz7j7nxaw7qi48dq8mn2w227ff8jdd4269cjf0sjvvpszjrhff";
    }) {};
    network = hsuper.network_3_1_1_0;
  };
};

{
  haskell = super.haskell // {
    packages = super.haskell.packages // {
      ghc881 = super.haskell.packages.ghc881.override {
        overrides = lib.composeExtensions
          (super.haskell.packageOverrides or (_self: _super: {}))
          mainOverlay;
      };
    };
  };
}
