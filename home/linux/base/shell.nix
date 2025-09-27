{
  config,
  myvars,
  ...
}:
let
  dh = config.xdg.dataHome;
  ch = config.xdg.configHome;
  chh = config.xdg.cacheHome;
in
rec {
  home.homeDirectory = "/home/${myvars.username}";

  home.sessionVariables = {
    lesshistfile = chh + "/less/history";
  };
}
