{ pkgs, config, ... }:
{
  hardware = {
    bluetooth = {
      enable = true;
      powerOnBoot = true;
      settings = {
        General = {
          ControllerMode = "dual";
          FastConnectable = true;
          Experimental = true;
        };
      };
    };
    keyboard.qmk.enable = true;
    graphics.enable = true;
    uinput.enable = true; # for kanata
  };
}
