.DELETE_ON_ERROR:
.ONESHELL:

SHELL := /bin/sh

# Tools and paths.
EMACS ?= emacs
EMACS_BATCH := $(EMACS) -Q --batch

BUILD_FILE := Makefile
DIST_DIR := dist
LISP_DIR := lisp
TESSERA_DIR := $(LISP_DIR)/tessera
TESSERA_GNUS_DIR := $(LISP_DIR)/tessera-gnus
PACKAGES := tessera tessera-gnus
ARCHIVE_TARGETS := $(addsuffix -archive,$(PACKAGES))

# Core package files.
TESSERA_MAIN := $(TESSERA_DIR)/tessera.el
TESSERA_PKG := $(TESSERA_DIR)/tessera-pkg.el
TESSERA_AUTOLOADS := $(TESSERA_DIR)/tessera-autoloads.el
TESSERA_ARCHIVE_STAMP := $(DIST_DIR)/.tessera-archive
TESSERA_LISP_FILES := $(filter-out \
	$(TESSERA_PKG) $(TESSERA_AUTOLOADS), \
	$(wildcard $(TESSERA_DIR)/*.el))
TESSERA_ELC_FILES := $(TESSERA_LISP_FILES:.el=.elc)

# Gnus package files.
TESSERA_GNUS_MAIN := $(TESSERA_GNUS_DIR)/tessera-gnus.el
TESSERA_GNUS_PKG := \
	$(TESSERA_GNUS_DIR)/tessera-gnus-pkg.el
TESSERA_GNUS_AUTOLOADS := \
	$(TESSERA_GNUS_DIR)/tessera-gnus-autoloads.el
TESSERA_GNUS_ARCHIVE_STAMP := \
	$(DIST_DIR)/.tessera-gnus-archive
TESSERA_GNUS_LISP_FILES := $(filter-out \
	$(TESSERA_GNUS_PKG) $(TESSERA_GNUS_AUTOLOADS), \
	$(wildcard $(TESSERA_GNUS_DIR)/*.el))
TESSERA_GNUS_ELC_FILES := \
	$(TESSERA_GNUS_LISP_FILES:.el=.elc)

# Generated files.
PKG_FILES := $(TESSERA_PKG) $(TESSERA_GNUS_PKG)
AUTOLOAD_FILES := \
	$(TESSERA_AUTOLOADS) \
	$(TESSERA_GNUS_AUTOLOADS)
ELC_FILES := \
	$(TESSERA_ELC_FILES) \
	$(TESSERA_GNUS_ELC_FILES)
ARCHIVE_STAMPS := \
	$(TESSERA_ARCHIVE_STAMP) \
	$(TESSERA_GNUS_ARCHIVE_STAMP)
GENERATED_FILES := \
	$(PKG_FILES) \
	$(AUTOLOAD_FILES) \
	$(ELC_FILES)

# Batch Emacs expressions.
# bake-format off
define GENERATE_PKG_ELISP
(progn
  (require (quote package))
  (let ((source
         (expand-file-name (getenv "PACKAGE_SOURCE")))
        (output
         (expand-file-name (getenv "PACKAGE_OUTPUT"))))
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

.PHONY: \
	all \
	archives \
	clean \
	tessera \
	tessera-pkg \
	tessera-autoloads \
	tessera-compile \
	tessera-archive \
	tessera-gnus \
	tessera-gnus-pkg \
	tessera-gnus-autoloads \
	tessera-gnus-compile \
	tessera-gnus-archive

all: tessera tessera-gnus

tessera: \
	tessera-pkg \
	tessera-autoloads \
	tessera-compile \
	tessera-archive

tessera-pkg: $(TESSERA_PKG)

tessera-autoloads: $(TESSERA_AUTOLOADS)

tessera-compile: $(TESSERA_ELC_FILES)

tessera-archive: $(TESSERA_ARCHIVE_STAMP)

tessera-gnus: \
	tessera-gnus-pkg \
	tessera-gnus-autoloads \
	tessera-gnus-compile \
	tessera-gnus-archive

tessera-gnus-pkg: $(TESSERA_GNUS_PKG)

tessera-gnus-autoloads: $(TESSERA_GNUS_AUTOLOADS)

tessera-gnus-compile: $(TESSERA_GNUS_ELC_FILES)

tessera-gnus-archive: $(TESSERA_GNUS_ARCHIVE_STAMP)

archives: $(ARCHIVE_TARGETS)

$(TESSERA_PKG): $(TESSERA_MAIN) $(BUILD_FILE)
$(TESSERA_GNUS_PKG): $(TESSERA_GNUS_MAIN) $(BUILD_FILE)

$(PKG_FILES):
	@env \
		PACKAGE_SOURCE="$<" \
		PACKAGE_OUTPUT="$@" \
		$(EMACS_BATCH) \
		--eval '$(GENERATE_PKG_ELISP)'

$(TESSERA_AUTOLOADS): \
	$(TESSERA_LISP_FILES) \
	$(BUILD_FILE)
$(TESSERA_GNUS_AUTOLOADS): \
	$(TESSERA_GNUS_LISP_FILES) \
	$(BUILD_FILE)

$(AUTOLOAD_FILES):
	@set -eu
	package_name="$(patsubst %-autoloads.el,%,$(notdir $@))"
	temp_dir=$$(mktemp -d)
	trap 'rm -rf "$$temp_dir"' EXIT HUP INT TERM
	cp $(filter %.el,$^) "$$temp_dir/"
	env \
		PACKAGE_DIR="$$temp_dir" \
		PACKAGE_NAME="$$package_name" \
		$(EMACS_BATCH) \
		--eval '$(GENERATE_AUTOLOADS_ELISP)'
	cp "$$temp_dir/$${package_name}-autoloads.el" "$@"

$(TESSERA_ELC_FILES) &: \
	$(TESSERA_LISP_FILES) \
	$(BUILD_FILE)
	$(EMACS_BATCH) \
		-L $(TESSERA_DIR) \
		--eval '(setq byte-compile-error-on-warn t)' \
		-f batch-byte-compile $(TESSERA_LISP_FILES)

$(TESSERA_GNUS_ELC_FILES) &: \
	$(TESSERA_GNUS_LISP_FILES) \
	$(TESSERA_ELC_FILES) \
	$(BUILD_FILE)
	$(EMACS_BATCH) \
		-L $(TESSERA_DIR) \
		-L $(TESSERA_GNUS_DIR) \
		--eval '(setq byte-compile-error-on-warn t)' \
		-f batch-byte-compile $(TESSERA_GNUS_LISP_FILES)

$(TESSERA_ARCHIVE_STAMP): \
	$(TESSERA_LISP_FILES) \
	$(TESSERA_PKG) \
	COPYING \
	$(BUILD_FILE)
$(TESSERA_GNUS_ARCHIVE_STAMP): \
	$(TESSERA_GNUS_LISP_FILES) \
	$(TESSERA_GNUS_PKG) \
	COPYING \
	$(BUILD_FILE)

$(ARCHIVE_STAMPS):
	@set -eu
	package_name="$(patsubst .%-archive,%,$(notdir $@))"
	version=$$(env \
		PACKAGE_SOURCE="$<" \
		$(EMACS_BATCH) \
		--eval '$(PACKAGE_VERSION_ELISP)')
	package_dir="$${package_name}-$${version}"
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
		PACKAGE_NAME="$$package_name" \
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