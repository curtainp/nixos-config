{
  config,
  pkgs,
  ...
}:
{
  security.polkit.enable = true;
  services.gnome = {
    gnome-keyring.enable = true;
    gcr-ssh-agent.enable = false;
  };
  programs.seahorse.enable = true;
  # use ssh-add to add key to agent
  programs.ssh.startAgent = true;
  security.pam.services.greetd.enableGnomeKeyring = true;

  programs.gnupg.agent = {
    enable = true;
    pinentryPackage = pkgs.pinentry-qt;
    enableSSHSupport = false;
    settings.default-cache-ttl = 5 * 60 * 60;
  };
}
