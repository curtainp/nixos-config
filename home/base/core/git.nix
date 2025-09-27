{
  config,
  lib,
  pkgs,
  myvars,
  ...
}:
{
  home.activation.removeExistingGitconfig = lib.hm.dag.entryBefore [ "checkLinkTargets" ] ''
    rm -f ${config.home.homeDirectory}/.gitconfig
  '';

  programs.git = {
    enable = true;
    userName = myvars.username;
    userEmail = myvars.useremail;
    lfs.enable = true;

    extraConfig = {
      # FOSS-friendly settings
      push.default = "simple"; # Match modern push behavior
      pull.rebase = true;
      credential.helper = "cache --timeout=7200";
      init.defaultBranch = "main"; # Set default new branches to 'main'
      log.decorate = "full"; # Show branch/tag info in git log
      log.date = "iso"; # ISO 8601 date format
      # Conflict resolution style for readable diffs
      merge.conflictStyle = "diff3";
    };
    difftastic = {
      enable = true;
      enableAsDifftool = true;
      background = "dark";
    };
    # Optional: FOSS-friendly Git aliases
    aliases = {
      br = "branch --sort=-committerdate";
      co = "checkout";
      df = "diff";
      com = "commit -a";
      gs = "stash";
      gp = "pull";
      lg = "log --graph --pretty=format:'%Cred%h%Creset - %C(yellow)%d%Creset %s %C(green)(%cr)%C(bold blue) <%an>%Creset' --abbrev-commit";
      st = "status";
    };
  };
}
