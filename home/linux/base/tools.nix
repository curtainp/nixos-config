{ pkgs, ... }:
{
  home.packages = with pkgs; [
    libnotify
  ];
  services.udiskie.enable = true; # for usb mount
}
