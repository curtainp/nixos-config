{
  lib,
  pkgs,
  pkgs-unstable,
  ...
}:
{
  home.packages = with pkgs; [
    pavucontrol
    playerctl
    pulsemixer
    imv

    libva-utils
    vdpauinfo
    glxinfo
    nvitop
  ];
  programs.obs-studio = {
    enable = pkgs.stdenv.isx86_64;
    plugins =
      with pkgs.obs-studio-plugins;
      [
        wlrobs
        obs-teleport
        obs-vkcapture
        obs-gstreamer
        input-overlay
        obs-source-clone
        obs-shaderfilter
        obs-source-record
        obs-livesplit-one
        looking-glass-obs
        obs-move-transition
        obs-backgroundremoval
        obs-pipewire-audio-capture
      ]
      ++ (lib.optionals pkgs.stdenv.isx86_64 [
        obs-vaapi
        obs-3d-effect
      ]);
  };
}
