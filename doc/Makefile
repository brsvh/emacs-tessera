.DELETE_ON_ERROR:
.DEFAULT_GOAL := all

MAKEINFO ?= makeinfo
MAKEINFO_FLAGS ?=
HTML_CSS_URL ?= https://www.gnu.org/software/emacs/manual.css
OUTPUT_DIR ?= ../dist/manuals
PACKAGE ?=

MANUALS := tessera tessera-x

ifneq ($(strip $(PACKAGE)),)
ifeq ($(filter $(PACKAGE),$(MANUALS)),)
$(error Unknown manual: $(PACKAGE))
endif
MANUALS := $(PACKAGE)
endif

INFO_FILES := $(addprefix $(OUTPUT_DIR)/info/,$(addsuffix .info,$(MANUALS)))
HTML_FILES := $(addprefix $(OUTPUT_DIR)/html/,$(addsuffix .html,$(MANUALS)))

.PHONY: all info html clean

all: info html

info: $(INFO_FILES)

html: $(HTML_FILES)

$(OUTPUT_DIR)/info/%.info: %.texi fdl.texi Makefile
	@mkdir -p "$(@D)"
	$(MAKEINFO) $(MAKEINFO_FLAGS) -I . --no-split -o "$@" "$<"

$(OUTPUT_DIR)/html/%.html: %.texi fdl.texi htmlxref.cnf Makefile
	@mkdir -p "$(@D)"
	$(MAKEINFO) $(MAKEINFO_FLAGS) -I . --html --no-split \
		--css-ref="$(HTML_CSS_URL)" \
		-c HTMLXREF_MODE=file -c HTMLXREF_FILE=htmlxref.cnf \
		-o "$@" "$<"

clean:
	$(RM) $(INFO_FILES) $(HTML_FILES)