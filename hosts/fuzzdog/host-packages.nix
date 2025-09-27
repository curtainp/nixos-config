{ pkgs, ... }:
{

  environment.systemPackages = with pkgs; [
    # core tools must have
    fastfetch
    gnumake
    cmake
    autoconf
    clang
    libclang
    clang_multi
    glibc
    glibcInfo
    glibc_multi
    gcc
    libcxx
    libgcc
    libvterm
    man-pages-posix
    stdmanpages
    ncurses
    ncurses5
    nodejs
    pciutils
    sysfsutils
    pinentry-curses
    pinentry-emacs
    pinentry-qt
    pkg-config
    polkit
    python3
    docker-compose
    ungoogled-chromium
    linuxHeaders
    kdePackages.xdg-desktop-portal-kde
    kdePackages.kde-gtk-config
    xdg-desktop-portal
    xdg-utils
    xdg-desktop-portal-wlr
    xdg-desktop-portal-gtk
    ffmpeg
    gdb
    gifski # gif encoder
    file
    tree
    duf
    bat
    mpv
    ncmpcpp
    bc
    bear
    ninja
    sysstat
    git
    lrzsz # transfer protocol with sx sz rz
    pkg-config
    lshw
    lsof
    meson
    libnotify
    jq
    just
    git-lfs
    neovim
    wget
    curl
    socat
    nmap
    openssl
    gnutar
    zlib-ng
    htop
    dig
    difftastic
    direnv
    iotop
    iftop
    ripgrep
    fd
    fzf
    tmux
    zoxide
    keepassxc
    zip
    unzip
    p7zip
    tealdeer # fast tldr

    # misc
    mtr # network diagnostic tool
    appimage-run
    cachix
    qt6.full
    qt6.qtwebsockets
    kdePackages.qtwebsockets
    kdePackages.qtmultimedia
    wayland-utils
    easyeffects
    nvme-cli
    smartmontools # monitor the health of hard drivers
    nix-zsh-completions
    nixd # nix language server
    nixfmt-rfc-style
    nodePackages.bash-language-server
    nodePackages.prettier
    imagemagick
  ];
}
