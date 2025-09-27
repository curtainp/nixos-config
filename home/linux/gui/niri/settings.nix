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
      (plain "input" [
        (plain "keyboard" [
          (plain "xkb" [
            (leaf "layout" "us")
          ])
          (leaf "repeat-delay" 250)
          (leaf "repeat-rate" 50)
        ])

        (plain "touchpad" [
          (flag "tap")
          (flag "natural-scroll")
        ])

        (plain "mouse" [

        ])
      ])

      (plain "layout" [
        (plain "focus-ring" [
          (leaf "width" 4)
          (leaf "active-color" "#7fc8ff")
          (leaf "inactive-color" "#505050")
        ])

        (plain "border" [
          (flag "off")
          (leaf "width" 4)
          (leaf "active-color" "#7fc8ff")
          (leaf "inactive-color" "#505050")
        ])

        (plain "preset-column-widths" [
          (leaf "proportion" (1.0 / 3.0))
          (leaf "proportion" (1.0 / 2.0))
          (leaf "proportion" (2.0 / 3.0))
        ])

        (plain "default-column-width" [
          (leaf "proportion" 0.5)
        ])

        (leaf "gaps" 12)

        (leaf "center-focused-column" "never")
      ])

      # (plain "cursor" [
        # Change the theme and size of the cursor as well as set the
        # `XCURSOR_THEME` and `XCURSOR_SIZE` env variables.
        # (leaf "xcursor-theme" "default")
        # (leaf "xcursor-size" 24)
      # ])

      # Uncomment this line to ask the clients to omit their client-side decorations if possible.
      # If the client will specifically ask for CSD, the request will be honored.
      # Additionally, clients will be informed that they are tiled, removing some rounded corners.
      # (flag "prefer-no-csd")

      # You can change the path where screenshots are saved.
      # A ~ at the front will be expanded to the home directory.
      # The path is formatted with strftime(3) to give you the screenshot date and time.
      (leaf "screenshot-path" "~/Pictures/screenshots/%Y-%m-%d_%H-%M-%S.png")

      # You can also set this to null to disable saving screenshots to disk.
      # (leaf "screenshot-path" null)

      # Settings for the "Important Hotkeys" overlay.
      (plain "hotkey-overlay" [
        # Uncomment this line if you don't want to see the hotkey help at niri startup.
        (flag "skip-at-startup")
      ])

      (plain "layer-rule" [
        (leaf "match" { namespace = "waybar"; })
        (leaf "opacity" 0.8)
      ])
    ];
}
