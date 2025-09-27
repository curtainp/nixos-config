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
      (leaf "spawn-at-startup" [ "swaybg -i ~/Pictures/wallpapers/mountainscapedark.jpg" ])
    ];
}
