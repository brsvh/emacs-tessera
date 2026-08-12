.DELETE_ON_ERROR:
.ONESHELL:

SHELL := /bin/sh

EMACS ?= emacs
EMACS_BATCH := $(EMACS) -Q --batch

BUILD_FILE := Makefile
DIST_DIR := dist
LISP_DIR := lisp
PACKAGE := tessera
MAIN := $(LISP_DIR)/$(PACKAGE).el
PKG := $(LISP_DIR)/$(PACKAGE)-pkg.el
AUTOLOADS := $(LISP_DIR)/$(PACKAGE)-autoloads.el
ARCHIVE_STAMP := $(DIST_DIR)/.$(PACKAGE)-archive
LISP_FILES := $(filter-out $(PKG) $(AUTOLOADS),$(wildcard $(LISP_DIR)/*.el))
ELC_FILES := $(LISP_FILES:.el=.elc)
GENERATED_FILES := $(PKG) $(AUTOLOADS) $(ELC_FILES)

# bake-format off
define GENERATE_PKG_ELISP
(progn
  (require (quote package))
  (let ((source (expand-file-name (getenv "PACKAGE_SOURCE")))
        (output (expand-file-name (getenv "PACKAGE_OUTPUT"))))
    (with-current-buffer (find-file-noselect source)
      (package-generate-description-file
       (package-buffer-info)
       output))))
endef

define GENERATE_AUTOLOADS_ELISP
(progn
  (require (quote package))
  (package-generate-autoloads
   (getenv "PACKAGE_NAME")
   (getenv "PACKAGE_DIR")))
endef

define PACKAGE_VERSION_ELISP
(progn
  (require (quote package))
  (with-current-buffer
      (find-file-noselect
       (expand-file-name (getenv "PACKAGE_SOURCE")))
    (princ
     (package-version-join
      (package-desc-version
       (package-buffer-info))))))
endef

define CHECK_ARCHIVE_ELISP
(progn
  (require (quote package))
  (let ((archive (getenv "PACKAGE_ARCHIVE"))
        (name (intern (getenv "PACKAGE_NAME")))
        (version (getenv "PACKAGE_VERSION")))
    (with-current-buffer (find-file-noselect archive)
      (let ((desc (package-tar-file-info)))
        (unless
            (and
             (eq (package-desc-name desc) name)
             (equal
              (package-version-join
               (package-desc-version desc))
              version))
          (error "Archive metadata mismatch"))))))
endef
# bake-format on

.PHONY: all archive autoloads clean compile package

all: package autoloads compile archive

package: $(PKG)

autoloads: $(AUTOLOADS)

compile: $(ELC_FILES)

archive: $(ARCHIVE_STAMP)

$(PKG): $(MAIN) $(BUILD_FILE)
	@env \
		PACKAGE_SOURCE="$<" \
		PACKAGE_OUTPUT="$@" \
		$(EMACS_BATCH) \
		--eval '$(GENERATE_PKG_ELISP)'

$(AUTOLOADS): $(LISP_FILES) $(BUILD_FILE)
	@set -eu
	temp_dir=$$(mktemp -d)
	trap 'rm -rf "$$temp_dir"' EXIT HUP INT TERM
	cp $(filter %.el,$^) "$$temp_dir/"
	env \
		PACKAGE_DIR="$$temp_dir" \
		PACKAGE_NAME="$(PACKAGE)" \
		$(EMACS_BATCH) \
		--eval '$(GENERATE_AUTOLOADS_ELISP)'
	cp "$$temp_dir/$(PACKAGE)-autoloads.el" "$@"

$(ELC_FILES) &: $(LISP_FILES) $(BUILD_FILE)
	$(EMACS_BATCH) \
		-L $(LISP_DIR) \
		--eval '(setq byte-compile-error-on-warn t load-prefer-newer t)' \
		-f batch-byte-compile $(LISP_FILES)

$(ARCHIVE_STAMP): $(LISP_FILES) $(PKG) COPYING $(BUILD_FILE)
	@set -eu
	version=$$(env \
		PACKAGE_SOURCE="$(MAIN)" \
		$(EMACS_BATCH) \
		--eval '$(PACKAGE_VERSION_ELISP)')
	package_dir="$(PACKAGE)-$${version}"
	archive="$(DIST_DIR)/$${package_dir}.tar"
	temp_dir=$$(mktemp -d)
	trap 'rm -rf "$$temp_dir"' EXIT HUP INT TERM
	mkdir -p "$$temp_dir/$$package_dir" "$(DIST_DIR)"
	cp $(filter %.el,$^) "$$temp_dir/$$package_dir/"
	cp COPYING "$$temp_dir/$$package_dir/"
	chmod -R u=rwX,go=rX "$$temp_dir/$$package_dir"
	tar \
		--sort=name \
		--mtime=@0 \
		--owner=0 \
		--group=0 \
		--numeric-owner \
		-cf "$$temp_dir/$${package_dir}.tar" \
		-C "$$temp_dir" \
		"$$package_dir"
	env \
		PACKAGE_ARCHIVE="$$temp_dir/$${package_dir}.tar" \
		PACKAGE_NAME="$(PACKAGE)" \
		PACKAGE_VERSION="$$version" \
		$(EMACS_BATCH) \
		--eval '$(CHECK_ARCHIVE_ELISP)'
	old_archive=
	if test -f "$@"; then
		IFS= read -r old_archive < "$@" || :
	fi
	mv "$$temp_dir/$${package_dir}.tar" "$$archive"
	if test -n "$$old_archive" \
	&& test "$$old_archive" != "$$archive"; then
		$(RM) "$$old_archive"
	fi
	printf '%s\n' "$$archive" > "$@"
	printf 'Created %s\n' "$$archive" >&2

clean:
	$(RM) $(GENERATED_FILES)
	rm -rf "$(DIST_DIR)"