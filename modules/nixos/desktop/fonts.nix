{ pkgs, ... }:
{
  fonts = {
    enableDefaultPackages = false;
    fontDir.enable = true;

    fontconfig = {
      defaultFonts = {
        serif = [
          "Source Sans 3"
          "Source Han Serif SC"
          "Source Han Serif TC"
        ];
        sansSerif = [
          "Source Serif 4"
          "LXGW WenKai Screen"
          "Source Han Sans SC"
          "Source Han Sans TC"
        ];
        monospace = [
          "JetBrainsMono Nerd Font"
          "Maple Mono NF CN"
          "Source Han Mono SC"
          "Source Han Mono TC"
        ];
        emoji = [ "Noto Color Emoji" ];
      };
      antialias = true;
      hinting.enable = false;
      subpixel = {
        rgba = "rgb";
      };
    };
  };

  services.kmscon = {
    enable = true;
    fonts = with pkgs; [
      {
        name = "Maple Mono NF CN";
        package = maple-mono.NF-CN-unhinted;
      }
      {
        name = "JetBrainsMono Nerd Font";
        package = nerd-fonts.jetbrains-mono;
      }
    ];
    extraOptions = "--term xterm-256color";
    extraConfig = "font-size=14";
    hwRender = true;
  };
}
