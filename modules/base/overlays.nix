{ ida-pro-overlay, emacs-overlay, ... }@args:
{
  nixpkgs.overlays = [
    ida-pro-overlay.overlays.default
    emacs-overlay.overlays.default
  ];
}
