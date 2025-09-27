{
  networking = {
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
