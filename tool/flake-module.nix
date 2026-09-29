{
  inputs,
  ...
}:
let
  inherit (inputs)
    infix
    ;
in
{
  imports = [
    infix.flakeModules.devshell
  ];

  perSystem =
    {
      config,
      inputs',
      lib,
      pkgs,
      ...
    }:
    {
      devshells = {
        default = import ./devshells/default.nix {
          inherit
            lib
            pkgs
            ;

          elfmt = inputs'.elfmt.packages.elfmt;
        };
      };

      formatter = import ./formatter.nix {
        inherit
          lib
          pkgs
          ;

        treefmtConfig = config.devshells.default.files.treefmt;
      };
    };
}
