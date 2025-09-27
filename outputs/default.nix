{
  self,
  nixpkgs,
  ...
}@inputs:
let
  inherit (inputs.nixpkgs) lib; # => lib = inputs.nixpkgs.lib
  mylib = import ../lib { inherit lib; };
  myvars = import ../vars { inherit lib; };

  genSpecialArgs =
    system:
    inputs
    // {
      inherit mylib myvars;
      pkgs-unstable = import inputs.nixpkgs-unstable {
        inherit system;
        config.allowUnfree = true;
      };
      pkgs-stable = import inputs.nixpkgs-stable {
        inherit system;
        config.allowUnfree = true;
      };
      pkgs-x64 = import nixpkgs {
        system = "x86_64-linux";
        config.allowUnfree = true;
      };
    };

  args = {
    inherit
      inputs
      lib
      mylib
      myvars
      genSpecialArgs
      ;
  };

  nixosSystems = {
    x86_64-linux = import ./x86_64-linux (args // { system = "x86_64-linux"; });
    # TODO: add others architecture here
  };
  allSystems = nixosSystems;
  allSystemNames = builtins.attrNames allSystems; # => [ "x86_64-linux" "xxxx" ]
  nixosSystemValues = builtins.attrValues nixosSystems;

  forAllSystems = func: (nixpkgs.lib.genAttrs allSystemNames func);
in
{
  nixosConfigurations = lib.attrsets.mergeAttrsList (
    map (it: it.nixosConfigurations or { }) nixosSystemValues
  );

  formatter = forAllSystems (system: nixpkgs.legacyPackages.${system}.nixfmt);
}
