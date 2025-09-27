{
  config,
  lib,
  pkgs,
  pkgs-unstable,
  ...
}:
{
  boot.loader.timeout = lib.mkForce 10;

  environment.shells = with pkgs; [
    bashInteractive
    zsh
  ];
  users.defaultUserShell = pkgs.zsh;
  programs.zsh.enable = true;
  security.sudo.keepTerminfo = true;

  environment.variables = {
    TZ = "${config.time.timeZone}";
  };

  environment.systemPackages = with pkgs; [
    wl-clipboard
  ];

  services = {
    gvfs.enable = true; # mount, trash tools
    tumbler.enable = true; # thumbnail for image
    daed.enable = true;
  };
  programs = {
    dconf.enable = true;
    thunar = {
      enable = true;
      plugins = with pkgs.xfce; [
        thunar-archive-plugin
        thunar-volman
      ];
    };
  };
}
