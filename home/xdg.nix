{ pkgs, ... }:
{
  xdg = {
    enable = true;
    mime.enable = true;
    mimeApps = {
      enable = true;
    };
    portal = {
      enable = true;
      extraPortals = [ pkgs.kdePackages.xdg-desktop-portal-kde ];
      configPackages = [ pkgs.kdePackages.xdg-desktop-portal-kde ];
    };
  };
}
