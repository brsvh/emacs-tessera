{
  inputs,
  projectRoot,
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
      lib,
      pkgs,
      ...
    }:
    let
      inherit (lib)
        getExe
        ;
    in
    {
      apps = {
        emacs =
          let
            launcher = pkgs.callPackage (projectRoot + /tool/apps/emacs.nix) {
              inherit
                projectRoot
                ;
            };
          in
          {
            meta = {
              description = "Launch Emacs with the local project configuration";
            };

            program = getExe launcher;
            type = "app";
          };
      };

      devshells = {
        default = import ./devshells/default.nix {
          inherit
            lib
            pkgs
            projectRoot
            ;
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
