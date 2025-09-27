{ lib, ... }:
{
  # lib.genAttrs <list of keys> <function => key: return value>
  listToAttrs = lib.genAttrs;
  # TODO: complete the docs
  inherit (lib.attrsets) mapAttrs;

  inherit (lib.attrsets) mapAttrs';

  inherit (lib.attrsets) mergeAttrsList;

  inherit (lib.attrsets) foldAttrs;
}
