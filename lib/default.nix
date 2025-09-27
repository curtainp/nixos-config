# NOTE: all references of defaults and builtins functions can be found in https://noogle.dev
{ lib, ... }:
{
  nixosSystem = import ./nixosSystem.nix;

  attrs = import ./attrs.nix { inherit lib; };

  relativeToRoot = lib.path.append ../.;
  scanPaths =
    path:
    builtins.map (f: (path + "/${f}")) (
      builtins.attrNames (
        lib.attrsets.filterAttrs (
          path: _type:
          (_type == "directory") || ((path != "default.nix") && (lib.strings.hasSuffix ".nix" path))
        ) (builtins.readDir path)
      )
    );
}
