{ root, inputs, ... }:
{
  imports = [
    inputs.haskell-flake.flakeModule
  ];
  perSystem = { self', lib, config, pkgs, ... }:
    let
      ghcDefault = "ghc910";
      ghcVersions = [ "ghc927" "ghc96" ghcDefault ];
    in
    {
      # Our only Haskell project. You can have multiple projects, but this template
      # has only one.
      # See https://github.com/srid/haskell-flake/blob/master/example/flake.nix
      haskellProjects = lib.genAttrs ghcVersions (ghc: {
        # The base package set (this value is the default)
        basePackages = pkgs.haskell.packages.${ghc};

        # To avoid unnecessary rebuilds, we filter projectRoot:
        # https://community.flake.parts/haskell-flake/local#rebuild
        projectRoot = builtins.toString (lib.fileset.toSource {
          inherit root;
          fileset = lib.fileset.unions [
            (root + /src)
            (root + /benchmark)
            (root + /test)
            (root + /laop.cabal)
            (root + /LICENSE)
            (root + /README.md)
            (root + /CHANGELOG.md)
          ];
        });

        # Packages to add on top of `basePackages`
        packages = {
          # Add source or Hackage overrides here
          # (Local packages are added automatically)
          /*
        aeson.source = "1.5.0.0" # Hackage version
        shower.source = inputs.shower; # Flake input
          */
        };

        # Add your package overrides here
        settings = {
          laop = {
            stan = false;
            # haddock = false;
          };
          /*
        aeson = {
          check = false;
        };
          */
        };

        # Development shell configuration
        devShell = {
          hlsCheck.enable = false;
          mkShellArgs = {
            shellHook = ''
              export SHELL=/run/current-system/sw/bin/bash
            '';
          };
          tools = hp: {
            # needed to get on a GHC910 dev env
            hlint = null;
          };
        };

        # What should haskell-flake add to flake outputs?
        autoWire = [ "packages" "apps" "checks" ]; # Wire all but the devShell
      });

      # Default package & app.
      packages.default = self'.packages."${ghcDefault}-laop";
      apps.default = self'.apps."${ghcDefault}-laop";
    };
}
