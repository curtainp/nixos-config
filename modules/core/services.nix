{ ... }:
{
  services = {
    power-profiles-daemon.enable = false;
    desktopManager.plasma6.enable = true;
    libinput.enable = true;
    udev.extraRules = ''
      KERNEL=="uinput", MODE="0660", GROUP="uinput", OPTIONS+="static_node=uinput"
    '';
    xserver = {
      enable = true;
      autoRepeatInterval = 50;
      autoRepeatDelay = 250;
      xkb = {
        layout = "us";
      };
    };
    printing.enable = false;
    pulseaudio.enable = false;
    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      # If you want to use JACK applications, uncomment this
      #jack.enable = true;
      extraConfig.pipewire."92-low-latency" = {
        "context.properties" = {
          "default.clock.rate" = 48000;
          "default.clock.quantum" = 256;
          "default.clock.min-quantum" = 256;
          "default.clock.max-quantum" = 256;
        };
      };
      extraConfig.pipewire-pulse."92-low-latency" = {
        context.modules = [
          {
            name = "libpipewire-module-protocol-pulse";
            args = {
              pulse.min.req = "256/48000";
              pulse.default.req = "256/48000";
              pulse.max.req = "256/48000";
              pulse.min.quantum = "256/48000";
              pulse.max.quantum = "256/48000";
            };
          }
        ];
      };
    };
    daed.enable = true;

    kanata = {
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
  };

  # for kanata work
  systemd.services.kanata-internalKeyboard.serviceConfig = {
    SupplementaryGroups = [
      "input"
      "uinput"
    ];
  };
  users.groups.uinput = { };
}
