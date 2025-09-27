{
  config,
  lib,
  ...
}:
{
  nixpkgs.config.allowUnfree = lib.mkForce true;

  nix.gc = {
    automatic = lib.mkDefault true;
    dates = lib.mkDefault "weekly";
    options = lib.mkDefault "--delete-old-than 5d";
  };

  nix.settings.auto-optimise-store = true;
  nix.channel.enable = false; # we use flake instead
}
