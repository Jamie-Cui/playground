EMACS = emacs
LOADPATH = -L lisp

SRCS = lisp/opencode.el \
       lisp/opencode-config.el \
       lisp/opencode-api.el \
       lisp/opencode-session.el \
       lisp/opencode-tools.el \
       lisp/opencode-agent.el \
       lisp/opencode-ui.el

COMPILED = $(SRCS:.el=.elc)

.PHONY: all compile clean test help

all: compile

help:
	@echo "OpenCode Emacs Lisp Implementation"
	@echo ""
	@echo "Targets:"
	@echo "  compile   - Byte compile all Elisp files"
	@echo "  clean     - Remove compiled files"
	@echo "  test      - Run tests"
	@echo "  help      - Show this help message"

compile: $(COMPILED)

lisp/%.elc: lisp/%.el
	@echo "Compiling $<..."
	$(EMACS) -Q --batch $(LOADPATH) -f batch-byte-compile $<

clean:
	@echo "Cleaning compiled files..."
	rm -f $(COMPILED)

test: compile
	@echo "Running tests..."
	$(EMACS) -Q --batch $(LOADPATH) -l lisp/opencode-tests.el -f ert-run-tests-batch-and-exit
