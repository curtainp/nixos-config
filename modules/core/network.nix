{ pkgs, host, ... }:
{

  networking = {
    hostName = "${host}";
    extraHosts = ''
      127.0.0.1 localhost
    '';
    networkmanager = {
      enable = true;
      wifi.powersave = false;
      wifi.backend = "wpa_supplicant";
    };
    firewall = {
      enable = false;
    };
  };
}
