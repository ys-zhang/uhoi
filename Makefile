THIS_FILE := $(lastword $(MAKEFILE_LIST))
ROOT_DIR := $(dir $(THIS_FILE))

PAPER_BUILD_DIR := $(shell pwd)/build-pdf
PAPER_SRC_DIR := $(ROOT_DIR)/paper
PAPER_SRC_FILES := $(wildcard $(PAPER_SRC_DIR)/*.lhs)

paper: clean paper-lhs-tex paper-pdf

paper-lhs-tex: $(PAPER_SRC_FILES)
	@mkdir -p "$(PAPER_BUILD_DIR)"
	@for f in $^; do \
	  base=$$(basename $$f .lhs); \
	  lhs2tex --poly -o "$(PAPER_BUILD_DIR)/$$base.tex" "$$f"; \
	done

paper-pdf: paper-lhs-tex $(PAPER_BUILD_DIR)/*.tex
	@pdflatex -output-directory "$(PAPER_BUILD_DIR)" \
	          -interaction=nonstopmode \
						-jobname=uhoi \
						$(PAPER_BUILD_DIR)/Main.tex
	

clean:
	@rm -rf "$(PAPER_BUILD_DIR)"


.PHONY: clean
