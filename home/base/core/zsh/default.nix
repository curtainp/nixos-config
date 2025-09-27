{
  pkgs,
  lib,
  config,
  ...
}:
{

  programs.zsh = {
    enable = true;
    autosuggestion.enable = true;
    syntaxHighlighting.enable = true;
    enableCompletion = true;
    defaultKeymap = "emacs";
    envExtra = ''
      zstyle ':completion:*' menu no # use fzf-tab instead
      zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'
      zstyle ':fzf-tab:complete:cd:*' fzf-preview 'eza -1 --color=always $realpath'
      zstyle ':fzf-tab:*' switch-group '<' '>'
    '';
    initContent = ''
      source ${pkgs.zsh-fzf-tab}/share/fzf-tab/fzf-tab.zsh
      source ${pkgs.zsh-autopair}/share/zsh/zsh-autopair/autopair.zsh
    '';
    shellAliases = {
      v = "nvim";
      mv = "mv -i";
      cp = "cp -i";
      rm = "rm -Iv";
      p = "python3";
      icat = "kitten icat";
      cat = "bat -p";
    };
    history = {
      size = 10000000;
      save = 10000000;
      append = true;
      expireDuplicatesFirst = true;
      saveNoDups = true;
      share = true;
      findNoDups = true;
      ignoreAllDups = true;
      ignoreDups = true;
      ignoreSpace = true;
      path = "${config.xdg.dataHome}/zsh/history";
    };
    plugins = [
      {
        name = "powerlevel10k";
        src = pkgs.zsh-powerlevel10k;
        file = "share/zsh-powerlevel10k/powerlevel10k.zsh-theme";
      }
      {
        name = "powerlevel10k-config";
        src = ./p10k;
        file = "p10k.zsh";
      }
    ];
  };
}
