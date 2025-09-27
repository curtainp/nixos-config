{
  pkgs,
  ...
}:
{
  home.packages =
    with pkgs;
    [
      wireshark
      mitmproxy
      keepassxc
      discord

      ffmpeg-full
      viu
      imagemagick
      graphviz
    ]
    ++ (lib.optionals pkgs.stdenv.isx86_64 [
      insomnia
    ]);
}
