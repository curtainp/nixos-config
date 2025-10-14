{
  pkgs,
  config,
  myvars,
  ...
}:
{
  nix.package = pkgs.lix;

  nix.settings = {
    experimental-features = [
      "nix-command"
      "flakes"
    ];
    trusted-users = [ myvars.username ];
    substituters = [
      "https://nix-community.cachix.org"
      "https://walker.cachix.org"
    ];
    trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "walker.cachix.org-1:fG8q+uAaMqhsMxWjwvk0IMb4mFPFLqHjuvfwQxE4oJM="
    ];
    builders-use-substitutes = true;
  };
}
