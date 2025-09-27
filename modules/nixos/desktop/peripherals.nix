{ pkgs, ... }:
{
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;
    wireplumber.enable = true;
  };
  security.rtkit.enable = true;
  services.pulseaudio.enable = false;

  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  services = {
    printing.enable = true;
    udev.packages = with pkgs; [
      gnome-settings-daemon
      android-udev-rules
    ];
  };

  # kanata key remapper
  boot.kernelModules = [ "uinput" ];
  hardware.uinput.enable = true;

  services.udev.extraRules = ''
    KERNEL=="uinput", MODE="0660", GROUP="uinput", OPTIONS+="static_node=uinput"
  '';
  systemd.services.kanata-internalKeyboard.serviceConfig = {
    SupplementaryGroups = [
      "input"
      "uinput"
    ];
  };
  services.kanata = {
    enable = true;
    keyboards = {
      internalKeyboard = {
        devices = [ ]; # lets kanata auto detect keyboard devices and intercept them all.
        extraDefCfg = "process-unmapped-keys yes";
        config = ''
          (defsrc
           a s d f j k l ;
          )
          (defvar
           tap-time 150
           hold-time 200
          )

          (defalias
           a (multi f24 (tap-hold $tap-time $hold-time a lmet))
           s (multi f24 (tap-hold $tap-time $hold-time s lalt))
           d (multi f24 (tap-hold $tap-time $hold-time d lsft))
           f (multi f24 (tap-hold $tap-time $hold-time f lctl))
           j (multi f24 (tap-hold-release-timeout 200 500 j rctl j))
           k (multi f24 (tap-hold-release-timeout 200 500 k rsft k))
           l (multi f24 (tap-hold $tap-time $hold-time l ralt))
           ; (multi f24 (tap-hold $tap-time $hold-time ; rmet))
          )

          (deflayer base
           @a @s @d @f @j @k @l @;
          )
        '';
      };
    };
  };
}
