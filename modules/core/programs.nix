{
  pkgs,
  inputs,
  ...
}:
{
  programs = {
    neovim = {
      enable = true;
      defaultEditor = true;
    };
    firefox.enable = false; # Firefox is not installed by default
    dconf.enable = true;
    fuse.userAllowOther = true;
    kdeconnect.enable = true;
    xwayland.enable = true;
    adb.enable = true;
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
      pinentryPackage = pkgs.pinentry-qt;
      enableExtraSocket = true;
    };
  };
}
