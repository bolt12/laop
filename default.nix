{ compiler ? "ghc92" }:

let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};

  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hself: hsuper: {
      "laop" =
        hself.callCabal2nix
          "laop"
          (gitignore ./.)
          {};
    };
  };

  shell = myHaskellPackages.shellFor {
    packages = p: [
      p."laop"
    ];
    buildInputs = with pkgs.haskellPackages; [
      # cabal
      cabal-install
      (import sources.niv {}).niv
      pkgs.nixpkgs-fmt
      (pkgs.haskell-language-server.override {
        supportedGhcVersions = [ "92" ];
      })
    ];
    withHoogle = false; # If 'true' it gives an error because the library only depends on
                        # 'base'
  };

  exe = pkgs.haskell.lib.justStaticExecutables (myHaskellPackages."laop");

  docker = pkgs.dockerTools.buildImage {
    name = "laop";
    config.Cmd = [ "${exe}/bin/laop" ];
  };
in
{
  inherit shell;
  inherit exe;
  inherit docker;
  inherit myHaskellPackages;
  "laop" = myHaskellPackages."laop";
}
