{
  ...
}:
{
  programs.ghostty = {
    enable = true;
    settings = {
      font-family = "Iosevka Nerd Font";
      font-codepoint-map = "U+4E00-U+9FFF,U+3400-U+4DBF = LXGW WenKai Screen";
      font-size = 15;
      macos-option-as-alt = true;
      cursor-style = "block";
      cursor-style-blink = false;
      window-decoration = "server";
      fullscreen = false;
      macos-non-native-fullscreen = "visible-menu";
    };
  };
}
