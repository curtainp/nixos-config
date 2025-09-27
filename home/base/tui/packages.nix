{
  pkgs,
  pkgs-unstable,
  ...
}:
{
  home.packages = with pkgs; [
    podman-compose
    pandoc
    pkgs-unstable.hugo
    cmake
    gcc
    gdb
    clang-tools

    uv
    pyright
    (python313.withPackages (
      ps: with ps; [
        ruff
        black
        ipython
        requests
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
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
    enableZshIntegration = true;
  };

  programs.ssh = {
    enable = true;
    addKeysToAgent = "yes";
    matchBlocks = {
      "github.com" = {
        hostname = "ssh.github.com";
        port = 443;
        user = "git";
        identitiesOnly = true;
      };
    };
  };
}
