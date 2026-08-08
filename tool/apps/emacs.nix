{
  coreutils,
  emacs,
  emacsPackagesFor,
  git,
  lib,
  mu,
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
      elfeed
      mood-line
      modus-themes
      mu4e
      nerd-icons
      tessera-gnus
    ]
  );

  initTemplate = projectRoot + /test/init.el.in;
in
writeShellApplication {
  name = "emacs";

  runtimeInputs = [
    coreutils
    git
    mu
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

    if [ ! -f "$runtimeProjectRoot/test/init.el.in" ]; then
      printf '%s\n' \
        'emacs-tessera: test/init.el.in is missing from the project checkout' \
        >&2
      exit 1
    fi

    initDirectory="$runtimeProjectRoot/local"
    initFile="$initDirectory/init.el"

    mkdir -p -- "$initDirectory/news" "$initDirectory/feeds"

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
