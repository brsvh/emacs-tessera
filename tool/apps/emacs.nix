{
  coreutils,
  emacs,
  emacsPackagesFor,
  git,
  lib,
  projectRoot,
  writeShellApplication,
}:
let
  inherit (lib)
    getExe
    ;

  emacsWithPackages = (emacsPackagesFor emacs).withPackages (
    emacsPackages: with emacsPackages; [
      better-defaults
      modus-themes
      tessera
    ]
  );

  initTemplate = projectRoot + /test/init.el.in;
in
writeShellApplication {
  name = "emacs";

  runtimeInputs = [
    coreutils
    git
  ];

  text = ''
    if ! runtimeProjectRoot="$(
      ${getExe git} -C "$PWD" rev-parse --show-toplevel 2>/dev/null
    )"; then
      printf '%s\n' \
        'emacs-tessera: run this app inside the project checkout' \
        >&2
      exit 1
    fi

    initDirectory="$runtimeProjectRoot/local"
    initFile="$initDirectory/init.el"

    mkdir -p -- "$initDirectory"

    if [ -e "$initFile" ] || [ -L "$initFile" ]; then
      if [ ! -f "$initFile" ]; then
        printf 'emacs-tessera: %s is not a regular file\n' "$initFile" >&2
        exit 1
      fi
    else
      install -m 0644 -- "${initTemplate}" "$initFile"
      printf 'emacs-tessera: created %s\n' "$initFile" >&2
    fi

    exec ${emacsWithPackages}/bin/emacs \
      --init-directory "$initDirectory" \
      "$@"
  '';
}
