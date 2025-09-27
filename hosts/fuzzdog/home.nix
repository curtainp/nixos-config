{
  config,
  niri,
  ...
}:
{
  modules.desktop.nvidia.enable = true;

  modules.desktop.niri = {
    settings =
      let
        inherit (niri.lib.kdl)
          node
          plain
          leaf
          flag
          ;
      in
      [
        (node "output" "DP-2" [
          (leaf "mode" "3840x2160@60")
          (leaf "transform" "normal")
          (leaf "scale" "1.25")
        ])
      ];
  };
}
