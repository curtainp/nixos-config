{
  pkgs,
  config,
  ...
}:
{
  home.pointerCursor = {
    gtk.enable = true;
    x11.enable = true;
    package = pkgs.bibata-cursors;
    name = "Bibata-Modern-Ice";
    size = 24;
  };

  xresources.properties = {
    "Xft.dpi" = 150;
    "*.dpi" = 150;
  };
  gtk = {
    enable = true;
    font = {
      name = "Noto Sans";
      package = pkgs.noto-fonts;
      size = 12;
    };
    gtk2.configLocation = "${config.xdg.configHome}/gtk-2.0/gtkrc";
  };
}
