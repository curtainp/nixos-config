{ ida-pro-overlay, ... }@args:
{
  nixpkgs.overlays = [
    ida-pro-overlay.overlays.default
  ];
}
