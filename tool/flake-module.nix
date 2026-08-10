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

      mkEmacsApp =
        emacs: description:
        let
          launcher = pkgs.callPackage (projectRoot + /tool/apps/emacs.nix) {
            inherit
              emacs
              projectRoot
              ;
          };
        in
        {
          meta = {
            inherit
              description
              ;
          };

          program = getExe launcher;
          type = "app";
        };
    in
    {
      apps = {
        emacs = mkEmacsApp pkgs.emacs "Launch Emacs with the local project configuration";

        emacs31 = mkEmacsApp pkgs.emacs31 "Launch Emacs 31 with the local project configuration";
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
