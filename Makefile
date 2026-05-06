# =====================================================================
# Shiny ML & Forecasting Lab - cross-platform Makefile
# Usage: make install | make run | make check | make clean
# =====================================================================

.PHONY: install run check clean help fmt

PORT ?= 4848
HOST ?= 127.0.0.1

help:
	@echo "make install  - install required R packages"
	@echo "make run      - launch the Shiny app on http://$(HOST):$(PORT)"
	@echo "make check    - parse-check every R file"
	@echo "make clean    - remove the local SQLite database"

install:
	Rscript install_R.R

run:
	Rscript -e "options(shiny.port=$(PORT), shiny.host='$(HOST)'); Sys.setenv(SHINYML_HOME=normalizePath('.')); shiny::runApp('.', launch.browser = TRUE)"

check:
	@for f in app.R R/*.R; do \
	  echo "== $$f =="; \
	  Rscript -e "tryCatch({parse('$$f'); cat('OK\n')}, error=function(e) {cat('PARSE ERROR:', conditionMessage(e), '\n'); quit(status=1)})"; \
	done

clean:
	rm -rf db/app.sqlite
	@echo "Removed local SQLite database."
