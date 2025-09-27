{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    strace
    ltrace
    tcpdump
    lsof

    bpftrace
    bpftop
    bpfmon

    sysstat
    iotop-c
    iftop
    btop
    nmon
    sysbench

    psmisc
    lm_sensors
    pciutils
    usbutils
    dmidecode

    nixfmt
  ];

  programs.bcc.enable = true;
}
