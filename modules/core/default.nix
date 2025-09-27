{ inputs, ... }:
{
  imports = [
    ./boot.nix
    ./fonts.nix
    ./hardware.nix
    ./network.nix
    ./nfs.nix
    ./nh.nix
    ./programs.nix
    ./services.nix
    ./sddm.nix
    ./stylix.nix
    ./system.nix
    ./user.nix
    ./virtualisation.nix
    inputs.stylix.nixosModules.stylix
  ];
}
