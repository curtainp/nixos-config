{
  pkgs,
  ...
}:
{
  programs.google-chrome = {
    enable = true;
    commandLineArgs = [
      "--ozone-platform-hint=auto"
      "--ozone-platform=wayland"
      "--gtk-version=4"
      "--enable-wayland-ime"
    ];
  };
}
