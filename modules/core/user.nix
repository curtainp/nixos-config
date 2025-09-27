{
  pkgs,
  username,
  ...
}:
{
  users.mutableUsers = true;
  users.users.${username} = {
    isNormalUser = true;
    extraGroups = [
      "adbusers"
      "docker" # access to docker as non-root
      "libvirtd" # Virt manager/QEMU access
      "video"
      "lp"
      "networkmanager"
      "scanner"
      "wheel" # subdo access
    ];
    shell = pkgs.zsh;
    ignoreShellProgramCheck = true;
  };
  nix.settings.allowed-users = [ "${username}" ];
}
