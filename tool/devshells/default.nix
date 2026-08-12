{
  lib,
  pkgs,
  projectRoot,
  ...
}:
let
  inherit (lib)
    concatStringsSep
    getExe
    ;

  inherit (lib.generators)
    toINIWithGlobalSection
    ;

  inherit (pkgs)
    writeText
    ;

  inherit (pkgs.formats)
    toml
    ;

  elfmt = pkgs.callPackage (projectRoot + /tool/elfmt/package.nix) {
    inherit
      projectRoot
      ;
  };

  formatters = with pkgs; [
    elfmt
    mbake
    nixfmt
  ];
in
{
  files = {
    editorconfig = rec {
      data = {
        root = true;

        "*" = {
          charset = "utf-8";
          end_of_line = "lf";
          indent_size = 8;
          indent_style = "tab";
          insert_final_newline = true;
          max_line_length = 70;
          tab_width = 8;
          trim_trailing_whitespace = true;
        };

        "{*.el,*.el.in}" = {
          indent_style = "space";
          indent_size = "unset";
          tab_width = 2;
        };

        "*.nix" = {
          indent_style = "space";
          max_line_length = 80;
          tab_width = 2;
        };

        "{Makefile,**.mk}" = {
          indent_size = 4;
          indent_style = "tab";
        };
      };

      generator =
        data:
        let
          name = baseNameOf path;

          value = {
            globalSection = {
              root = data.root or true;
            };

            sections = removeAttrs data [
              "root"
            ];
          };
        in
        writeText name (toINIWithGlobalSection { } value);

      packages = with pkgs; [
        editorconfig-checker
      ];

      path = ".editorconfig";
    };

    prek = rec {
      data = {
        default_install_hook_types = [
          "pre-commit"
        ];

        repos = [
          {
            repo = "local";

            hooks = [
              {
                entry = "treefmt --fail-on-change";
                id = "treefmt";
                language = "system";
                name = "treefmt";

                stages = [
                  "pre-commit"
                ];
              }
            ];
          }
        ];
      };

      deps = [
        "treefmt"
      ];

      generator = data: (toml { }).generate (baseNameOf path) data;

      hook =
        let
          inherit (pkgs)
            git
            prek
            runtimeShell
            writeScript
            ;

          mkInstall = stage: ''
            if gitDir="$(
              ${getExe git} -C "$PRJ_ROOT" \
                rev-parse --absolute-git-dir \
                2>/dev/null
            )"; then
              mkdir -p "$gitDir/hooks"
              ln -sf "${mkScript stage}" "$gitDir/hooks/${stage}"
            fi
          '';

          mkScript =
            stage:
            writeScript "prek-${stage}" ''
              #!${runtimeShell}
              if [ "''${PREK:-}" = "0" ] || [ "''${LEFTHOOK:-}" = "0" ]; then
                exit 0
              fi

              gitDir="$(
                ${getExe git} -C "$PRJ_ROOT" \
                  rev-parse --absolute-git-dir \
                  2>/dev/null || true
              )"

              if [ -n "$gitDir" ]; then
                if [ -e "$gitDir/MERGE_HEAD" ] \
                  || [ -d "$gitDir/rebase-apply" ] \
                  || [ -d "$gitDir/rebase-merge" ]; then
                  exit 0
                fi

                ref="$(
                  ${getExe git} -C "$PRJ_ROOT" \
                    symbolic-ref --quiet --short HEAD \
                    2>/dev/null || true
                )"

                if [ "$ref" = "update_flake_lock_action" ]; then
                  exit 0
                fi
              fi

              exec ${getExe prek} -C "$PRJ_ROOT" run --stage "${stage}" "$@"
            '';
        in
        concatStringsSep "\n" (map mkInstall data.default_install_hook_types);

      packages = with pkgs; [
        git
        prek
      ];

      path = "prek.toml";
    };

    treefmt = rec {
      data = {
        formatter = {
          emacs-lisp = {
            command = "elfmt";

            includes = [
              "*.el"
              "*.el.in"
            ];
          };

          makefile = {
            command = "mbake";

            includes = [
              "*.Makefile"
              "*.makefile"
              "*.mk"
              "*/Makefile"
              "*/makefile"
              "Makefile"
              "Makefile.*"
              "makefile"
              "makefile.*"
            ];

            options = [
              "format"
            ];
          };

          nix = {
            command = "nixfmt";

            includes = [
              "*.nix"
            ];

            options = [
              "--width=80"
            ];
          };
        };
      };

      generator = data: (toml { }).generate (baseNameOf path) data;

      packages = formatters ++ [
        pkgs.treefmt
      ];

      path = "treefmt.toml";
    };
  };

  packages = with pkgs; [
    gnumake
    gnutar
  ];
}
