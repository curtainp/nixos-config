{
  config,
  pkgs,
  ...
}:
{
  home.packages = with pkgs; [
    xdg-utils
    xdg-user-dirs
  ];
  xdg.configFile."mimeapps.list".force = true;
  xdg = {
    enable = true;

    cacheHome = "${config.home.homeDirectory}/.cache";
    configHome = "${config.home.homeDirectory}/.config";
    dataHome = "${config.home.homeDirectory}/.local/share";
    stateHome = "${config.home.homeDirectory}/.local/state";

    mimeApps = {
      enable = true;

      defaultApplications =
        let
          browser = [
            "google-chrome.desktop"
            "firefox.desktop"
          ];
          editor = [
            # TODO: add Emacs
            "nvim.desktop"
          ];
        in
        {
          "application/json" = browser;
          "application/pdf" = browser; # TODO: pdf viewer

          "text/html" = browser;
          "text/xml" = browser;
          "text/plain" = editor;
          "application/xml" = browser;
          "application/xhtml+xml" = browser;
          "application/xhtml_xml" = browser;
          "application/rdf+xml" = browser;
          "application/rss+xml" = browser;
          "application/x-extension-htm" = browser;
          "application/x-extension-html" = browser;
          "application/x-extension-shtml" = browser;
          "application/x-extension-xht" = browser;
          "application/x-extension-xhtml" = browser;
          "application/x-wine-extension-ini" = editor;

          # define default applications for some url schemes.
          "x-scheme-handler/about" = browser; # open `about:` url with `browser`
          "x-scheme-handler/ftp" = browser; # open `ftp:` url with `browser`
          "x-scheme-handler/http" = browser;
          "x-scheme-handler/https" = browser;
          # all other unknown schemes will be opened by this default application.
          # "x-scheme-handler/unknown" = editor;

          "x-scheme-handler/tg" = [ "org.telegram.desktop.desktop " ];

          "audio/*" = [ "mpv.desktop" ];
          "video/*" = [ "mpv.desktop" ];
          "image/*" = [ "imv-dir.desktop" ];
          "image/gif" = [ "imv-dir.desktop" ];
          "image/jpeg" = [ "imv-dir.desktop" ];
          "image/png" = [ "imv-dir.desktop" ];
          "image/webp" = [ "imv-dir.desktop" ];

          "inode/directory" = [ "yazi.desktop" ];
        };
    };

    userDirs = {
      enable = true;
      createDirectories = true;
      extraConfig = {
        XDG_SCREENSHOTS_DIR = "${config.xdg.userDirs.pictures}/screenshots";
      };
    };
  };
}
