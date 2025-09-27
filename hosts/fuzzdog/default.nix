{ myvars, ... }:
let
  hostName = "fuzzdog";
in
{
  imports = [
    ./hardware-configuration.nix
    ./nvidia.nix
  ];

  networking = {
    inherit hostName;
  };

  system.stateVersion = "25.05";
}
