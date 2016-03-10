SHELL = bash

default: link

link:
	find global -type f -printf '%d %p\n' \
	|while read -r d f; do \
	  r=..; \
	  while [[ $$d > 1 ]]; do \
	    let d--; \
	    r="$$r/.."; \
	  done; \
	  for p in *; do \
	    [[ -f $$p/prefs.js ]] || continue; \
	    ln -sf "$$r/$$f" "$$p/$${f#*/}"; \
	  done; \
	done

