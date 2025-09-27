{
  pkgs,
  config,
  lib,
  niri,
  ...
}:
let
  cfg = config.modules.desktop.niri;
in
{
  options.modules.desktop.niri = {
    enable = lib.mkEnableOption "niri compositor";
    settings = lib.mkOption {
      type =
        with lib.types;
        let
          valueType =
            nullOr (oneOf [
              bool
              int
              float
              str
              path
              (attrOf valueType)
              (listOf valueType)
            ])
            // {
              decription = "niri configuration value";
            };
        in
        valueType;
      default = { };
    };
  };

  config = lib.mkIf cfg.enable (
    lib.mkMerge [
      {
        home.file.".wayland-session" = {
          source = pkgs.writeScript "init-session" ''
            systemctl --user is-active niri.service && systemctl --user stop niri.service
            /run/current-system/sw/bin/niri-session
          '';
          executable = true;
        };
      }
      (import ./settings.nix niri)
      (import ./keybindings.nix niri)
      (import ./startup.nix niri)
      # (import ./windowrules.nix niri)
    ]
  );
}
