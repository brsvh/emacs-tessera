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
                    description,
                    lib,
                    melpaBuild,
                    packageRequires ? [ ],
                    pname,
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
                    inherit
                      packageRequires
                      pname
                      ;

                    files = ''("lisp/${pname}/*.el" "doc/${pname}.texi")'';

                    meta = {
                      inherit
                        description
                        ;

                      homepage = "https://github.com/brsvh/emacs-tessera";
                      license = licenses.gpl3Plus;
                      maintainers = with maintainers; [ brsvh ];
                    };

                    src = projectRoot;
                    version = "0.1.0";
                  };

                scope = finalAttrs: _: {
                  tessera = finalAttrs.callPackage emacs-tessera {
                    inherit
                      projectRoot
                      ;

                    description = "Modern interfaces for Emacs communication tools";

                    packageRequires = with finalAttrs; [
                      alert
                    ];

                    pname = "tessera";
                  };

                  tessera-x = finalAttrs.callPackage emacs-tessera {
                    inherit
                      projectRoot
                      ;

                    description = "Shared experimental features for Tessera";

                    packageRequires = with finalAttrs; [
                      tessera
                    ];

                    pname = "tessera-x";
                  };

                  tessera-x-elfeed = finalAttrs.callPackage emacs-tessera {
                    inherit
                      projectRoot
                      ;

                    description = "Experimental Tessera features for Elfeed";

                    packageRequires = with finalAttrs; [
                      elfeed
                      tessera
                      tessera-x
                    ];

                    pname = "tessera-x-elfeed";
                  };

                  tessera-x-gnus = finalAttrs.callPackage emacs-tessera {
                    inherit
                      projectRoot
                      ;

                    description = "Experimental Tessera features for Gnus";

                    packageRequires = with finalAttrs; [
                      tessera
                      tessera-x
                    ];

                    pname = "tessera-x-gnus";
                  };

                  tessera-x-mu4e = finalAttrs.callPackage emacs-tessera {
                    inherit
                      projectRoot
                      ;

                    description = "Experimental Tessera features for Mu4e";

                    packageRequires = with finalAttrs; [
                      mu4e
                      tessera
                      tessera-x
                    ];

                    pname = "tessera-x-mu4e";
                  };
                };
              in
              {
                emacsPackagesFor = p: (emacsPackagesFor p).overrideScope scope;
              };
          };
        };

        partitionedAttrs = {
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
