{
  pkgs,
  config,
  ...
}:
let
  patched-ida-pro = with pkgs;
  (
    (callPackage ida-pro {
      # TODO: use fetchurl to get installer from remote
      runfile = builtins.fetchurl {
      	url = "https://gfs208n140.userstorage.mega.co.nz/dl/47uBbmMGqc6YggIOLPrEEUkOSwmoNaXePh-KW02HQn0V0xV3idautfcQZENZr-GdZgnPYLjBqNGI3f1FCIT3BcNHaIRwEhNVe_sujtTSkRFgQWHlmrLJDWfdpp9g4MD8I_d8i082Q4fYj9SRkfi6981pb2aXeA/ida-pro_92_x64linux.run";
	      sha256 = "sha256:1qass0401igrfn14sfrvjfyz668npx586x59yaa4zf3jx650zpda";
      };
    }).overrideAttrs
    (
     prev: final: {
      installPhase =
        builtins.replaceStrings
          [ 
            ''for lib in $IDADIR/*.so $IDADIR/*.so.6; do''
            ''# Manually patch libraries that dlopen stuff.''
          ]
          [
             ''
              cd $IDADIR
              python3 ${ ./patch.py }
              mv idapro.hexlic $out
              for lib in $IDADIR/*.so $IDADIR/*.so.6; do
             ''

             ''
              rm $out/opt/libida.so $out/opt/libida32.so
              mv libida.so.patched $out/opt/libida.so
              mv libida32.so.patched $out/opt/libida32.so
             ''
          ]
           final.installPhase;
         }
     )
  );
in
{
  home.file.".idapro/idapro.hexlic".source = "${patched-ida-pro}/idapro.hexlic";
  home.packages = [
    patched-ida-pro
  ];
}
