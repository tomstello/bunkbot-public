# Bunkbot reproduction pipeline.
#
#   data/ (raw + cached API) -> code/R/build_all_numbers.R -> ALL_NUMBERS -> {results_methods.Rmd, supplement/Bunkbot_SI.Rmd} -> output/
#
# Two public documents, both driven by the SAME live recompute (build_all_numbers()):
#   results_methods.Rmd                    main-text Methods + Results (analysis code exposed)
#   supplement/Bunkbot_SI.Rmd              the Supplementary Information (analyses that map
#                                          directly onto the paper + their robustness tests)
# Quick start:
#   make results            render results_methods.Rmd  (HTML + PDF + Word) to output/
#   make supplement         render the FOCAL supplement (HTML + PDF + Word) to output/
#   make test               run the developer regression check (dev_qa.R)
#   make help               list targets
#
# Reproduction is API-FREE: everything reads the committed files under data/.

RSCRIPT ?= Rscript
.PHONY: help results supplement figures html pdf word test scan clean regenerate-api manuscript-check manuscript-wire code-ocean

help:
	@printf "Bunkbot reproduction targets:\n"
	@printf "  make results              %s\n" "render results_methods.Rmd -> HTML + PDF + Word (output/)"
	@printf "  make supplement           %s\n" "render the FOCAL supplement -> HTML + PDF + Word (output/)"
	@printf "  make figures              %s\n" "rebuild the 3 main-text data figures -> figures/manuscript/"
	@printf "  make html|pdf|word        %s\n" "render the focal SI in a single format"
	@printf "  make test                 %s\n" "developer regression check (code/dev_qa.R)"
	@printf "  make manuscript-check     %s\n" "check the Word manuscript's numbers against the recompute"
	@printf "  make manuscript-wire      %s\n" "write '<manuscript> (wired).docx' with corrections as tracked changes"
	@printf "  make code-ocean          %s\n" "run the Code Ocean driver locally -> code_ocean_results/"
	@printf "  make scan                 %s\n" "gitleaks secret scan (.gitleaks.toml)"
	@printf "  make clean                %s\n" "remove rendered output/"
	@printf "  make regenerate-api       %s\n" "opt-in, key-gated regeneration of cached annotations (COSTS MONEY)"

results:
	$(RSCRIPT) -e "rmarkdown::render('results_methods.Rmd', output_format = 'all', output_dir = 'output')"

supplement:
	$(RSCRIPT) -e "rmarkdown::render('supplement/Bunkbot_SI.Rmd', output_format = 'all', output_dir = 'output')"

figures:
	$(RSCRIPT) figures/manuscript/make_figures.R

html:
	$(RSCRIPT) -e "rmarkdown::render('supplement/Bunkbot_SI.Rmd', output_format = 'bookdown::html_document2', output_dir = 'output')"

pdf:
	$(RSCRIPT) -e "rmarkdown::render('supplement/Bunkbot_SI.Rmd', output_format = 'bookdown::pdf_document2', output_dir = 'output')"

word:
	$(RSCRIPT) -e "rmarkdown::render('supplement/Bunkbot_SI.Rmd', output_format = 'bookdown::word_document2', output_dir = 'output')"

test:
	$(RSCRIPT) code/dev_qa.R

code-ocean:
	RESULTS_DIR="$(CURDIR)/code_ocean_results" bash code/run

# Manuscript wiring: refresh the value manifest from the live recompute, then
# check (drift report) or redline (tracked-changes copy; original never touched).
manuscript-check:
	$(RSCRIPT) code/manuscript_numbers.R
	python3 code/manuscript_wiring/check_manuscript.py check

manuscript-wire:
	$(RSCRIPT) code/manuscript_numbers.R
	python3 code/manuscript_wiring/check_manuscript.py redline

scan:
	gitleaks dir . --config .gitleaks.toml --no-banner

clean:
	@find output -type f ! -name '.gitkeep' -delete

regenerate-api:
	@echo "Cached model annotations under data/api_cached/ regenerate via the provenance"
	@echo "pipelines in code/provenance/python_api/. This requires API keys and COSTS MONEY."
	@echo "Copy prompts/.env.example to .env, set the keys, then run the per-pipeline scripts."
	@echo "The default reproduction is API-free and does NOT need this."
