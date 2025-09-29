{
  pkgs,
  pkgs-unstable,
  ...
}:
{
  home.packages = with pkgs; [
    pwntools
    hashcat

    scrypt # for android

    man-pages
    man-pages-posix

    podman-compose
    pandoc
    pkgs-unstable.hugo
    cmake
    gcc
    clang-tools

    uv
    basedpyright
    (python313.withPackages (
      ps: with ps; [
        capstone
        ruff
        black
        ipython
        requests
        epc
        orjson
        sexpdata
        six
        setuptools
        paramiko
        rapidfuzz
        watchdog
        packaging
      ]
    ))

    pkgs-unstable.rustc
    pkgs-unstable.rust-analyzer
    pkgs-unstable.cargo
    pkgs-unstable.rustfmt
    pkgs-unstable.clippy

    nodePackages.bash-language-server
    shellcheck
    shfmt

    nodePackages.nodejs
    nodePackages.typescript
    nodePackages.typescript-language-server
    nodePackages.prettier

    # TODO: db related
  ];

  services.dropbox.enable = true;
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
    enableZshIntegration = true;
  };

  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;
    matchBlocks = {
      "github.com" = {
        hostname = "ssh.github.com";
        port = 443;
        user = "git";
        identitiesOnly = true;
        addKeysToAgent = "yes";
      };
    };
  };
}
