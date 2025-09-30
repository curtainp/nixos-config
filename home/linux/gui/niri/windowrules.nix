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
      (plain "window-rule" [
        (leaf "geometry-corner-radius" 5)
        (leaf "clip-to-geometry" true)
      ])
    ];
}
