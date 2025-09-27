{ ... }:
{
  programs.tealdeer = {
    enable = true;
    enableAutoUpdates = true;
    settings = {
      display = {
        compact = false;
        use_paper = true;
      };
      updates = {
        auto_update = false;
        auto_update_interval_hours = 720;
      };
    };
  };
}
