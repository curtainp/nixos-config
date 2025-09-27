{
  config,
  pkgs,
  ...
}:
{
  imports = [
    ./anyrun.nix
    ./nvidia.nix
  ];

  home.sessionVariables = {
    "NIXOS_OZONE_WL" = "1"; # for any ozone-based browser & electron apps to run on wayland
    "MOZ_ENABLE_WAYLAND" = "1"; # for firefox to run on wayland
    "MOZ_WEBRENDER" = "1";
    # enable native Wayland support for most Electron apps
    "ELECTRON_OZONE_PLATFORM_HINT" = "auto";
    # misc
    "_JAVA_AWT_WM_NONREPARENTING" = "1";
    "QT_WAYLAND_DISABLE_WINDOWDECORATION" = "1";
    "QT_QPA_PLATFORM" = "wayland";
    "SDL_VIDEODRIVER" = "wayland";
    "GDK_BACKEND" = "wayland";
    "XDG_SESSION_TYPE" = "wayland";
  };
  home.packages = with pkgs; [
    swaybg
    wl-clipboard
    brightnessctl
    hyprshot
    networkmanagerapplet
  ];
  xdg.configFile =
    let
      mkSymlink = config.lib.file.mkOutOfStoreSymlink;
      # NOTE: we hardcode the nixos path here!
      configPath = "${config.home.homeDirectory}/workspace/nixos-configuration/home/linux/gui/base/desktop/conf";
    in
    {
      "mako".source = mkSymlink "${configPath}/mako";
      "waybar".source = mkSymlink "${configPath}/waybar";
      "wlogout".source = mkSymlink "${configPath}/wlogout";
      "hypr/hypridle.conf".source = mkSymlink "${configPath}/hypridle.conf";
    };

  programs.waybar = {
    enable = true;
    systemd.enable = true;
  };
  programs.swaylock.enable = true;
  programs.wlogout.enable = true;
  services.hypridle.enable = true;
  services.mako.enable = true;
}
