{
  config,
  ...
}:
{
  programs.gpg = {
    enable = true;
    homedir = "${config.home.homeDirectory}/.gnupg";
    settings = {
      no-greeting = true;
      no-emit-version = true;
      no-comments = false;
      keyid-format = "0xlong";
      with-fingerprint = true;
      # use stronger default instead
      personal-cipher-preferences = "AES256";
      personal-digest-preferences = "SHA512";
      weak-digest = "SHA1";
    };
  };
}
