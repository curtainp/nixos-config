{
  pkgs,
  ...
}:
{
  i18n.inputMethod = {
    enable = true;
    type = "fcitx5";
    fcitx5 = {
      waylandFrontend = true;
      addons = with pkgs; [
        (fcitx5-rime.override { rimeDataPkgs = [ pkgs.rime-ice ]; })
        fcitx5-gtk
        fcitx5-nord
        kdePackages.fcitx5-configtool
        kdePackages.fcitx5-qt
      ];
      settings = {
        addons = {
          classicui.globalSection.Theme = "Nord-Dark";
          classicui.globalSection.DarkTheme = "Nord-Dark";
        };
        inputMethod = {
          "Groups/0" = {
            Name = "Default";
            "Default Layout" = "us";
            "DefaultIM" = "keyboard-us";
          };
          "Groups/0/Items/0".Name = "keyboard-us";
          "Groups/0/Items/1".Name = "rime";
        };
      };
    };
  };
}
