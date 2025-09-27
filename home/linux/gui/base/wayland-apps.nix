{
  pkgs,
  lib,
  ...
}:
{

    programs.chromium = {
      enable = true;
      package = pkgs.ungoogled-chromium;
      extensions =
        let
          createChromiumExtensionFor =
            browserVersion:
            {
              id,
              sha256,
              version,
            }:
            {
              inherit id;
              crxPath = builtins.fetchurl {
                url = "https://clients2.google.com/service/update2/crx?response=redirect&acceptformat=crx2,crx3&prodversion=${browserVersion}&x=id%3D${id}%26installsource%3Dondemand%26uc";
                name = "${id}.crx";
                inherit sha256;
              };
              inherit version;
            };
          createChromiumExtension = createChromiumExtensionFor (
            lib.versions.major pkgs.ungoogled-chromium.version
          );
        in
        [
          (createChromiumExtension {
            # ublock origin
            id = "cjpalhdlnbpafiamejdnhcphjbkeiagm";
            sha256 = "sha256:054kqrai2kd89bzc5c3x17rjfdil2zzxrxrg65vaywmvm77y7kmn";
            version = "1.66.4";
          })
          (createChromiumExtension {
            # darker reader
            id = "eimadpbcbfnmbkopoojfekhnkhdbieeh";
            sha256 = "sha256:1i8rs6bcblx4d85rh41pmky3hhlpzn5977lpz5zmhwri7sb77yzk";
            version = "4.9.110";
          })
          (createChromiumExtension {
            # saladict
            id = "cdonnmffkdaoajfknoeeecmchibpmkmg";
            sha256 = "sha256:1l6xvb58spi1x03458wq1yx32iys990kwaqwnnxwdq53kw3v1fvf";
            version = "7.20.0";
          })
          (createChromiumExtension {
            # keepassxc-browser
            id = "oboonakemofpalcgghocfoadofidjkkk";
            sha256 = "sha256:0hzcxgdv7xa97qvm1yc64agyg6pvhds0nfwhkx36ndk28db93w1p";
            version = "1.9.9.6";
          })
        ];
    };
}
