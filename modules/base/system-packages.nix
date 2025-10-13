{ pkgs, pwndbg, ... }:
{
  environment.variables.EDITOR = "nvim";

  environment.systemPackages = with pkgs; [
    tealdeer # faster tldr
    fastfetch
    neovim
    gnumake
    cmake
    autoconf
    pkg-config
    git
    git-lfs
    difftastic
    wget
    curl
    dig

    zip
    zstd
    unzipNLS
    p7zip

    gnugrep
    gnused
    gawk
    ripgrep
    fd
    bat
    bear
    ninja
    lrzsz
    jq
    fzf
    socat
    nmap
    direnv
    zoxide
    file
    xwayland-satellite
    nixd
    pwndbg.packages.${pkgs.system}.default
    gdb
  ];
  environment.localBinInPath = true;
}
