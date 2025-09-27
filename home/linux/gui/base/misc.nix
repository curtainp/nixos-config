{
  pkgs,
  ...
}:
{
  home.packages = with pkgs; [
    telegram-desktop
  ];
  fonts.fontconfig.enable = false;
}
