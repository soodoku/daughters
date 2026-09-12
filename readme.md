### Replication Materials For "Revisiting a Natural Experiment: Do Legislators With Daughters Vote More Liberally on Women's Issues?"

The historical manuscript abstract:

An intriguing natural experiment arises from the fact that legislators are randomly assigned some combination of sons or daughters. The pioneering work of Washington (2008) shows that legislators with daughters cast more liberal roll call votes on women's issues. Costa et al. (2019) find that this pattern subsides in more recent congresses and speculate that increasing party polarization might diminish the ``daughters effect.'' The present paper delves more deeply into patterns of change over time by looking at eight congresses prior to the four studied by Washington (2008) as well as eight subsequent congresses, including three not included in Costa et al. (2019). Contrary to the party polarization hypothesis, we find no daughters effect leading up to the period that Washington studied and no effect thereafter. The cohort of members whom Washington studied exhibit consistently positive effects over time, while other legislators exhibit non-positive effects.The daughters effect appears to be a statistical aberration.

### Manuscript

* [Manuscript](ms/ms.pdf)
* [Supporting Information](ms/si.pdf)

[Changes from the manuscript version](CHANGES.md) records issues, actions, numerical consequences and release tags.

### Data

1. [AAUW Data 97th--116th Congresses](https://doi.org/10.7910/DVN/HD5VHI)
2. [Costa et al. (2019) Replication Archive](data/costa_et_al/)
3. [Washington (2008) Replication Archive](data/washington)
4. [Congressional Member ID Data](data/member_id/)
5. [Voteview Data](data/voteview_congress_members.csv)
6. [Female Members of Congress](data/female%20members%20of%20congress.csv)
7. [Data on Children of MCs](data/Child%20Info%20Master%20List%20Dotters.csv)
8. [US Census Bureau Regions and Divisions](data/us_census_bureau_regions_and_divisions.csv) via [Chris](https://raw.githubusercontent.com/cphalpert/census-regions/master/us%20census%20bureau%20regions%20and%20divisions.csv)
9. [Literature Review](data/dotters_lit.csv)

### Reproduce

Run from the repository root. This revision was validated with R 4.6.0; `renv.lock` records the package versions, including the GitHub revision of `fwildclusterboot`.

```r
install.packages("renv")
renv::restore(library = "renv/library", prompt = FALSE)
```

```sh
make reproduce
make validate
make lint
```

`make reproduce` rebuilds the data, then overwrites the current tables in `tabs/` and figures in `figs/`. `make results` reruns the results using the supplied analytical CSV. `make validate` compares the current results with the historical Git tag and independently reconstructs the early vote scores; its CSVs and package versions go to `/tmp/daughters-comparisons` (override with `COMPARISONS=/your/path`). Keep the Git tags when cloning to run these comparisons.

| Scripts | Purpose |
|---|---|
| [01](scripts/01_official_cong_list.R), [02](scripts/02_aauw_full.R), [03](scripts/03_final_dataset_wrangle.R) | Build the member roster, AAUW scores and analytical CSV from the supplied sources. |
| [04](scripts/04_costa_et_al_rep.R), [05](scripts/05_washington_rep.R) | Reproduce the comparison specifications using Costa and Washington's original data. |
| [06](scripts/06_lit_review.R) | Build the literature table. |
| [07](scripts/07_balance_checks.R) | Balance checks and party model. |
| [08](scripts/08_daughters_paper_outputs_1.R) | Main table and cohort figure, cohort tables, pooled and non-biological-child specifications. |
| [09](scripts/09_daughters_paper_outputs_2.R) | Supplementary outcomes, any-daughter models and AAUW-by-party figure. |
| [validate](scripts/validate.R) | Historical reproduction, isolated corrections and optional specification comparisons. |

AAUW is stored on a 0–100 scale and analyzed on a 0–1 scale. Annual tables retain the manuscript's OLS standard errors; pooled CSVs and tables use 9,999 wild bootstrap draws clustered by legislator. Both random generators are seeded. The balance party model retains the original seeded selection of one Congress per legislator. Source column names are retained in the supplied data; analysis scripts use names such as `n_daughters`, `n_children` and `has_daughter`.

The PDFs in `ms/` are the historical manuscript and supplement. Editable manuscript source is absent from this checkout, so those PDFs do **not** incorporate the corrections. Current numerical replacements are in `tabs/` and `figs/`; [CHANGES.md](CHANGES.md) explains their consequences. Interpretation changes and time-varying child counts remain for a subsequent revision.

### Authors

[Donald Green](http://donaldgreen.com/), [Oliver Hyman-Metzger](https://github.com/olivermetzger), [Gaurav Sood](https://github.com/soodoku), and [Michelle A. Zee](https://github.com/michelleazee)
