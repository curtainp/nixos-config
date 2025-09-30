{
  config,
  pkgs,
  walker,
  ...
}:
{
  imports = [
    # ./anyrun.nix
    walker.homeManagerModules.default
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
    # hyprshot
    networkmanagerapplet
    (writeShellScriptBin "dmenu_bookmark" (builtins.readFile ./scripts/dmenu_bookmark))
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
      "emacs".source = mkSymlink "${configPath}/emacs.d";
      "bookmarks".source = mkSymlink "${configPath}/bookmarks";
      "hypr/hypridle.conf".source = mkSymlink "${configPath}/hypridle.conf";
    };

  programs.waybar = {
    enable = true;
    systemd.enable = true;
  };
  programs.swaylock = {
    enable = true;
    settings = {
	    ignore-empty-password = true;
	    show-failed-attempts = true;
	    image="$HOME/Pictures/wallpapers/mountainscapedark.jpg";
	    scaling="fill";
	    indicator-caps-lock = true;
	    indicator-radius=60;
	    indicator-thickness=10;
	    font="JetBrainsMono Nerd Font";
	    font-size=24;

	    key-hl-color="6f3f89";
	    bs-hl-color="ce104c";

	    ring-color="005577";

	    ring-clear-color="e3c401";
	    inside-clear-color="e3c401";
	    text-clear-color="191919";

	    ring-wrong-color="c85577";
	    inside-wrong-color="c85577";
	    text-wrong-color="191919";

	    ring-ver-color="a6e22e";
	    inside-ver-color="a6e22e";
	    text-ver-color="191919";

	    ring-caps-lock-color="13c299";
	    inside-caps-lock-color="13c299";
	    caps-lock-key-hl-color="6f3f89";
	    text-caps-lock-color="191919";
    };
  };
  programs.elephant = {
    enable = true;
    installService = true;
  };
  programs.walker = {
    enable = true;
    runAsService = true;
    config = {
      providers.desktopapplications.toggle_pin = "ctrl shift p";
      providers.prefixes = [
        { provider = "websearch"; prefix = "+"; }
      ];
      keybinds = {
        quick_activate = ["ctrl 1" "ctrl 2" "ctrl 3" "ctrl 4"];
        next = "ctrl n";
        previous = "ctrl p";
      };
    };
    # theme.style = ''
    # '';
  };
  programs.wlogout.enable = true;
  services.hypridle.enable = true;
  services.mako.enable = true;
}
