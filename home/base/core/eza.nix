{
  programs.eza = {
    enable = true;
    icons = "auto";
    enableBashIntegration = true;
    enableZshIntegration = true;
    # enableFishIntegration = true;
    git = true;

    extraOptions = [
      "--group-directories-first"
      "--no-quotes"
      "--git-ignore"
      "--icons=always"
      # "--time-style=long-iso" # ISO 8601 extended format for time
      "--classify" # append indicator (/, *, =, @, |)
      "--hyperlink" # make paths clickable in some terminals
    ];
  };
  # Aliases to make `ls`, `ll`, `la` use eza
  home.shellAliases = {
    ls = "eza";
    lt = "eza --tree --level=2";
    ll = "eza -lg --long";
    la = "eza -lah ";
    tree = "eza --tree ";
  };
}
