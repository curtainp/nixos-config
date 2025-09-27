{
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.modules.desktop;
in
{
  options.modules.desktop = {
    fonts.enable = lib.mkEnableOption "Rich Fonts - NerdFonts & Icons & Emoji & CJK Fonts";
  };

  config.fonts.packages =
    with pkgs;
    lib.mkIf cfg.fonts.enable [
      material-design-icons

      nerd-fonts.symbols-only
      nerd-fonts.jetbrains-mono
      nerd-fonts.iosevka
      nerd-fonts.symbols-only

      noto-fonts
      noto-fonts-color-emoji
      source-sans
      source-serif
      source-han-sans
      source-han-serif
      source-han-mono

      lxgw-wenkai-screen
      maple-mono.NF-CN-unhinted
    ];
}
