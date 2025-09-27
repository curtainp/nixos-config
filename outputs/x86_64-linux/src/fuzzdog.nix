# i5-13400f + RTX 4060 Ti
{
  inputs,
  lib,
  mylib,
  myvars,
  system,
  genSpecialArgs,
  niri,
  ...
}@args:
let
  name = "fuzzdog";
  base-modules = {
    nixos-modules =
      (map mylib.relativeToRoot [
        "modules/nixos/desktop.nix"
        "hosts/${name}"
      ])
      ++ [
        inputs.daeuniverse.nixosModules.daed
        inputs.niri.nixosModules.niri
        {
          modules.desktop.fonts.enable = true;
          modules.desktop.wayland.enable = true;
        }
      ];
    home-modules = map mylib.relativeToRoot [
      "home/linux/gui.nix"
      "hosts/${name}/home.nix"
    ];
  };
  modules-niri = {
    nixos-modules = [
      { programs.niri.enable = true; }
    ]
    ++ base-modules.nixos-modules;
    home-modules = [
      { modules.desktop.niri.enable = true; }
    ]
    ++ base-modules.home-modules;
  };
in
{
  nixosConfigurations = {
    "${name}" = mylib.nixosSystem (modules-niri // args);
  };
}
