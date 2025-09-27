{
  config,
  lib,
  pkgs,
  modulesPath,
  ...
}:
{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  boot = {
    loader.efi.canTouchEfiVariables = true;
    loader.systemd-boot.enable = true;
    initrd.availableKernelModules = [
      "xhci_pci"
      "ahci"
      "nvme"
      "usbhid"
      "usb_storage"
      "sd_mod"
    ];
    initrd.kernelModules = [ ];
    kernelModules = [
      "kvm-intel"
      # "nvidia_uvm"
      "uinput"
    ];
    extraModulePackages = [ ];
    blacklistedKernelModules = [ "nouveau" ];
  };

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/4a3bd8ec-0bdf-42a5-b807-0e8890d2b8e1";
      fsType = "ext4";
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/50EA-B0D9";
      fsType = "vfat";
      options = [
        "fmask=0077"
        "dmask=0077"
      ];
    };
    # TODO: add more mount point, like nas ?
  };

  swapDevices = [
    { device = "/dev/disk/by-uuid/4c682595-a1e0-4482-8b4a-a72cba1fe043"; }
  ];

  networking.useDHCP = lib.mkDefault true;
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  security.sudo.wheelNeedsPassword = false;
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
