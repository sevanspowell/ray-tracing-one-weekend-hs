{ compiler, withHoogle, withProfiling }:

self: super:

let
  hsPkgs = if compiler == "default"
           then super.haskellPackages
           else super.haskell.packages.${compiler};

  addHoogleOverlay = overlay:
                       self.lib.composeExtensions
                         overlay
                         (hself: hsuper: {
                           ghc = hsuper.ghc // { withPackages = hsuper.ghc.withHoogle; };
                           ghcWithPackages = hself.ghc.withPackages;
                         });

  addProfilingOverlay = overlay:
                          self.lib.composeExtensions
                            overlay
                            (hself: hsuper: {
                              mkDerivation = args: hsuper.mkDerivation (args // {
                                enableLibraryProfiling = true;
                              });
                            });

  maybeAddHoogle = if withHoogle
                   then addHoogleOverlay
                   else self.lib.id;

  maybeAddProfiling = if withProfiling
                      then addProfilingOverlay
                      else self.lib.id;

  hsPkgSources = {
  };

  hsPkgsOverlay = hself: hsuper: {
  };

in
  {
    haskellPackages = hsPkgs.override (oldArgs: {
      overrides =
        self.lib.composeExtensions
          (oldArgs.overrides or (_: _: {}))
          (maybeAddProfiling (maybeAddHoogle hsPkgsOverlay));
    });
  }
