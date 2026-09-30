{ inputs, ... }:
{
  imports = [
    (inputs.git-hooks-nix + /flake-module.nix)
  ];
  perSystem = {
    # The same formatters the editor applies: stylish-haskell (configured by
    # .stylish-haskell.yaml), cabal-gild and nixfmt.
    pre-commit.settings = {
      hooks = {
        stylish-haskell.enable = true;
        cabal-gild.enable = true;
        nixfmt.enable = true;
        hlint.enable = false;
      };
    };
  };
}
