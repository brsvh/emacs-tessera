{
  description = "A modern interface suite for elfeed, gnus, and mu4e";

  inputs = {
    flake-parts = {
      inputs = {
        nixpkgs-lib = {
          follows = "nixpkgs";
        };
      };

      url = "git+https://github.com/hercules-ci/flake-parts.git?ref=main";
    };

    nixpkgs = {
      url = "git+https://github.com/NixOS/nixpkgs.git?ref=nixos-unstable";
    };
  };

  outputs =
    inputs@{
      flake-parts,
      nixpkgs,
      self,
      ...
    }:
    let
      inherit (flake-parts.lib)
        mkFlake
        ;

      projectRoot = ./.;
    in
    mkFlake
      {
        inherit
          inputs
          ;

        specialArgs = {
          inherit
            projectRoot
            ;
        };
      }
      {
        imports = [
          flake-parts.flakeModules.partitions
        ];

        flake = {
          overlays = {
            default =
              final: prev:
              let
                inherit (prev)
                  emacsPackagesFor
                  ;

                emacs-tessera =
                  {
                    alert,
                    lib,
                    melpaBuild,
                    projectRoot,
                    ...
                  }:
                  let
                    inherit (lib)
                      licenses
                      maintainers
                      ;
                  in
                  melpaBuild {
                    files = ''("*.el")'';

                    packageRequires = [
                      alert
                    ];

                    meta = {
                      description = "Common foundation for Tessera interface packages";
                      homepage = "https://github.com/brsvh/emacs-tessera";
                      license = licenses.gpl3Plus;
                      maintainers = with maintainers; [ brsvh ];
                    };

                    pname = "tessera";
                    src = projectRoot + /lisp/tessera;
                    version = "0.1.0";
                  };

                emacs-tessera-gnus =
                  {
                    lib,
                    melpaBuild,
                    projectRoot,
                    tessera,
                    ...
                  }:
                  let
                    inherit (lib)
                      licenses
                      maintainers
                      ;
                  in
                  melpaBuild {
                    files = ''("*.el")'';

                    packageRequires = [
                      tessera
                    ];

                    meta = {
                      description = "Tessera interface integration for Gnus";
                      homepage = "https://github.com/brsvh/emacs-tessera";
                      license = licenses.gpl3Plus;
                      maintainers = with maintainers; [ brsvh ];
                    };

                    pname = "tessera-gnus";
                    src = projectRoot + /lisp/tessera-gnus;
                    version = "0.1.0";
                  };

                emacs-tessera-mu4e =
                  {
                    lib,
                    melpaBuild,
                    mu4e,
                    projectRoot,
                    tessera,
                    ...
                  }:
                  let
                    inherit (lib)
                      licenses
                      maintainers
                      ;
                  in
                  melpaBuild {
                    files = ''("*.el")'';

                    packageRequires = [
                      mu4e
                      tessera
                    ];

                    meta = {
                      description = "Tessera interface integration for mu4e";
                      homepage = "https://github.com/brsvh/emacs-tessera";
                      license = licenses.gpl3Plus;
                      maintainers = with maintainers; [ brsvh ];
                    };

                    pname = "tessera-mu4e";
                    src = projectRoot + /lisp/tessera-mu4e;
                    version = "0.1.0";
                  };

                scope = finalAttrs: _: {
                  tessera = finalAttrs.callPackage emacs-tessera {
                    inherit
                      projectRoot
                      ;
                  };

                  tessera-gnus = finalAttrs.callPackage emacs-tessera-gnus {
                    inherit
                      projectRoot
                      ;
                  };

                  tessera-mu4e = finalAttrs.callPackage emacs-tessera-mu4e {
                    inherit
                      projectRoot
                      ;
                  };
                };
              in
              {
                emacsPackagesFor = p: (emacsPackagesFor p).overrideScope scope;
              };
          };
        };

        partitionedAttrs = {
          apps = "tool";
          devShells = "tool";
          formatter = "tool";
        };

        partitions = {
          tool = {
            extraInputsFlake = projectRoot + /tool;

            module =
              {
                ...
              }:
              {
                imports = [
                  (projectRoot + /tool/flake-module.nix)
                ];
              };
          };
        };

        perSystem =
          {
            pkgs,
            system,
            ...
          }:
          {
            _module = {
              args = {
                pkgs = import nixpkgs {
                  inherit
                    system
                    ;

                  overlays = [
                    self.overlays.default
                  ];
                };
              };
            };
          };

        systems = [
          "x86_64-linux"
        ];
      };
}
