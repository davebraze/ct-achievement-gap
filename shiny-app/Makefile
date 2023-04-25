R.OPTS := --vanilla
APP.DIR = .
date := $(shell date "+%Y%m%d")

.phoney: shiny 
## Few targets correspond to files, so, list them here to ensure they always run.

help:
## Print Constants and Targets defined in this Makefile
	@echo Constants::
	@echo R.OPTS: $(R.OPTS)
	@echo APP.DIR: $(APP.DIR)
	@echo date: $(date)
	@echo
	@echo Targets::
	@echo --
	@grep -E -A 1 "^[^\# ]+:" Makefile
	@echo --
	@echo 

### building reports

start-shiny: 
# start shiny app in clean R session
	Rscript $(R.OPTS) -e "shiny::runApp('$(APP.DIR)')"

deploy-shiny:
# deploy shiny to shinyapps.io
	Rscript $(R.OPTS) -e   "rsconnect::deployApp(appName='naep-reading-gap', appDir='$(APP.DIR)')"
