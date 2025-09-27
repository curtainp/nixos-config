niri: {
  programs.niri.config =
    let
      inherit (niri.lib.kdl)
        node
        plain
        leaf
        flag
        ;
    in
    [
      (leaf "spawn-at-startup" [ "swaybg" "-o" "DP-2" "-i" "/home/curtain/Pictures/wallpapers/mountainscapedark.jpg" ])
    ];
}
