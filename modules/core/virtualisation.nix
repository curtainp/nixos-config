{ pkgs, ... }:
{
  virtualisation = {
    libvirtd.enable = true;
    waydroid.enable = true; # android emulator in Linux
    docker = {
      enable = true;
    };
  };
  # FIXME: separate this
  security.rtkit.enable = true;
  programs.virt-manager.enable = true;
}
