R = R_LIBS=renv/library Rscript --vanilla
COMPARISONS ?= /tmp/daughters-comparisons

.PHONY: reproduce data results validate lint

reproduce: data
	$(MAKE) results

data:
	$(R) scripts/01_official_cong_list.R
	$(R) scripts/02_aauw_full.R
	$(R) scripts/03_final_dataset_wrangle.R

results:
	$(R) scripts/04_costa_et_al_rep.R
	$(R) scripts/05_washington_rep.R
	$(R) scripts/06_lit_review.R
	$(R) scripts/07_balance_checks.R
	$(R) scripts/08_daughters_paper_outputs_1.R
	$(R) scripts/09_daughters_paper_outputs_2.R

validate:
	$(R) scripts/validate.R "$(COMPARISONS)"

lint:
	$(R) -e 'lints <- lintr::lint_dir("scripts"); print(lints); stopifnot(length(lints) == 0)'
