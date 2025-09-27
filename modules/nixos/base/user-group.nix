{
  myvars,
  config,
  ...
}:
{
  users.mutableUsers = false;

  users.groups = {
    "${myvars.username}" = { };
    wireshark = { };
    adbuser = { };
    dialout = { };
    uinput = { };
  };

  users.users."${myvars.username}" = {
    inherit (myvars) initialHashedPassword;
    home = "/home/${myvars.username}";
    isNormalUser = true;
    extraGroups = [
      myvars.username
      "users"
      "networkmanager"
      "wheel"
      "wireshark"
      "adbuser"
      "lp"
      "uinput"
      "libvirtd"
    ];
  };

  users.users.root = {
    inherit (myvars) initialHashedPassword;
  };
}
