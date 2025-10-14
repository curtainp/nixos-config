{ pkgs, ... }:
{
  programs.kitty = {
    enable = true;
    package = pkgs.kitty;
    font = {
      name = "Iosevka Nerd Font";
      size = 15;
    };
    themeFile = "Doom_One";
    settings = {
      macos_show_window_title_in = "none";
      macos_option_as_alt = true;
      wheel_scroll_min_lines = 1;
      window_padding_width = 4;
      confirm_os_window_close = 0;
      scrollback_lines = 100000;
      enable_audio_bell = false;
      mouse_hide_wait = 60;
      cursor_trail = 1;
      # tab_fade = 1;
      # active_tab_font_style = "bold";
      # inactive_tab_font_style = "bold";
      # tab_bar_edge = "top";
      # tab_bar_margin_width = 0;
      # tab_bar_style = "powerline";
      # tab_bar_style = "fade";
      # enabled_layouts = "splits";
    };
    extraConfig = ''
      # Clipboard
      map ctrl+shift+v        paste_from_selection
      map shift+insert        paste_from_selection
      map ctrl+c              copy_or_interrupt

      # Scrolling
      map ctrl+shift+up        scroll_line_up
      map ctrl+shift+down      scroll_line_down
      map ctrl+shift+k         scroll_line_up
      map ctrl+shift+j         scroll_line_down
      map ctrl+shift+page_up   scroll_page_up
      map ctrl+shift+page_down scroll_page_down
      map ctrl+shift+home      scroll_home
      map ctrl+shift+end       scroll_end
      map ctrl+shift+h         show_scrollback

      # Chinese font
      symbol_map U+4E00-U+9FFF,U+3400-U+4DBF LXGW WenKai Screen
    '';
  };
}
