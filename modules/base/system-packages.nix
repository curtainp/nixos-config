{ pkgs, ... }:
{
  environment.variables.EDITOR = "nvim --clean";

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

    (callPackage ida-pro {
      # TODO: use fetchurl to get installer from remote
      runfile = builtins.fetchurl {
      	url = "https://gfs208n140.userstorage.mega.co.nz/dl/47uBbmMGqc6YggIOLPrEEUkOSwmoNaXePh-KW02HQn0V0xV3idautfcQZENZr-GdZgnPYLjBqNGI3f1FCIT3BcNHaIRwEhNVe_sujtTSkRFgQWHlmrLJDWfdpp9g4MD8I_d8i082Q4fYj9SRkfi6981pb2aXeA/ida-pro_92_x64linux.run";
	sha256 = "sha256:1qass0401igrfn14sfrvjfyz668npx586x59yaa4zf3jx650zpda";
      };
    })
  ];
}
