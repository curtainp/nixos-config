{ pkgs, ... }:
{
  fonts = {
    fontDir = {
      enable = true;
      decompressFonts = true;
    };
    # Enable a basic set of fonts providing several styles and families and reasonable coverage of Unicode.
    enableDefaultPackages = true;
    packages = with pkgs; [
      symbola
      iosevka
      lxgw-wenkai
      nerd-fonts.jetbrains-mono
      nerd-fonts.symbols-only
      noto-fonts
      noto-fonts-cjk-sans
      noto-fonts-cjk-serif
      noto-fonts-emoji
      source-han-sans
      source-han-serif
      liberation_ttf
      material-icons
      dina-font
      proggyfonts
      aileron # font
    ];
    fontconfig = {
      enable = true;
      defaultFonts = {
        serif = [
          "Noto Serif CJK SC"
          "Noto Serif"
          "Noto Color Emoji"
          "Twitter Color Emoji"
        ];
        sansSerif = [
          "Noto Sans CJK SC"
          "Noto Color Emoji"
        ];
        monospace = [
          "JetBrainsMono Nerd Font"
          "Noto Color Emoji"
        ];
        emoji = [
          "Noto Color Emoji"
          "Noto Sans Egyptian Hieroglyphs"
        ];
      };
    };
  };
}
