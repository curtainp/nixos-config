{ pkgs, ... }:
let
  # FIXME: rime-ice is in unstable channel now.
  fcitx5-rime = pkgs.fcitx5-rime.override {
    rimeDataPkgs = [ ./fcitx5-rime-ice ];
  };
in
{
  nixpkgs = {
    # TODO: overlays here
    # overlays = [
    #   outputs.overlays.unstable-packages
    # ];
    config = {
      allowUnfree = true;
      cudaSupport = false;
      nvidia.acceptLicense = true;
      permittedInsecurePackages = [
        "electron-28.3.3"
      ];
    };
  };

  nix = {
    gc = {
      automatic = true;
      dates = "weekly";
    };
    settings = {
      auto-optimise-store = true; # delete duplicate files by hard link
      # ability to specify additional binary caches or unsigned NARs with nix daemon
      trusted-users = [
        "root"
        "curtain"
      ];

      trusted-public-keys = [
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        # "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw="
        # "chaotic-nyx.cachix.org-1:HfnXSw4pj95iI/n17rIDy40agHj12WfF+Gqk6SonIT8="
      ];
      substituters = [
        "https://cache.nixos.org"
        "https://nix-community.cachex.org"
        # "https://devenv.cachix.org"
        # "https://chaotic-nyx.cachix.org"
      ];
      experimental-features = [
        "nix-command"
        "flakes"
      ];
    };
  };

  time.timeZone = "Asia/Hong_Kong";

  i18n = {
    defaultLocale = "en_US.UTF-8";
    extraLocaleSettings = {
      LC_ALL = "en_US.UTF-8";
      LC_CTYPE = "en_US.UTF-8";
      LC_COLLATE = "en_US.UTF-8";
      LC_MESSAGES = "en_US.UTF-8";
      LC_ADDRESS = "en_US.UTF-8";
      LC_IDENTIFICATION = "en_US.UTF-8";
      LC_MEASUREMENT = "en_US.UTF-8";
      LC_MONETARY = "en_US.UTF-8";
      LC_NAME = "en_US.UTF-8";
      LC_NUMERIC = "en_US.UTF-8";
      LC_PAPER = "en_US.UTF-8";
      LC_TELEPHONE = "en_US.UTF-8";
      LC_TIME = "en_US.UTF-8";
    };

    inputMethod = {
      enable = true;
      type = "fcitx5";
      fcitx5 = {
        waylandFrontend = true;
        addons = with pkgs; [
          fcitx5-rime
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
  };

  environment = {
    variables = {
      NIXOS_OZONE_WL = "1";
      GDK_BACKEND = "wayland";
      GTK_IM_MODULE = "wayland";
      QT_IM_MODULE = "wayland";
      EDITOR = "nvim";
      VISUAL = "nvim";
      MOZ_ENABLE_WAYLAND = "1";
      LIBCLANG_PATH = "${pkgs.llvmPackages.libclang.lib}/lib";
      ELECTRON_OZONE_PLATFORM_HINT = "auto";
    };
    localBinInPath = true;
  };

  system.stateVersion = "25.05";
}
