# Changes from the manuscript version

The coding corrections below are implemented. AAUW effects here are points on its 0–100 scale; the analysis tables use 0–1. Comparisons refer to the local manuscript and supplement, whose equivalence to the final journal PDFs has not been certified.

| Issue | What changed | Numerical consequence |
|---|---|---|
| “Any daughter” meant two or more. Costa et al. and our SI define at least one. | Use `ngirls > 0` in data construction. | Reclassifies **3,441** member–Congress rows. Correcting this indicator alone moves the pooled estimate **1.58 → 0.83** points; after all fixes it is **0.87**, 95% interval **[−5.64, 7.38]**, p = .79. Congress 105 moves **13.0 → 6.39** points (p = **.008 → .27**). Main daughter-count estimates are unaffected by this indicator correction. |
| A redundant Voteview join added 14 observations. | Remove the join; retain the ideology measure already in the analytical data. | Main N **7,670 → 7,656**, still **1,459 legislators**. By itself, the pooled daughter-count estimate moves **1.929 → 1.933** points. |
| Early roll numbers were matched across chambers, and paired votes counted as votes. | Match House votes only; follow the SI's rule treating paired votes as abstentions. They remain in the all-roll-call denominator. | House matching changes one score (Akaka, Congress 101: **89 → 100**); paired voting changes another **41**. After both fixes and join removal, the pooled daughter-count estimate is **1.927**, 95% interval **[−0.96, 4.82]**, p = .19. |
| Washington-cohort membership included people without House service in Congresses 105–108. | Require House service; exclude rows without usable outcomes before fitting cohort models. | Membership **569 → 533** legislators; **128** analyzed rows switch cohorts. The slope difference outside Congresses 105–108 moves **9.28 → 9.14** points from the membership fix alone and to **9.12** after all coding fixes (clustered 95% interval **[3.88, 14.36]**). |
| SI 6.1 presents proportion-of-daughters coefficients under a per-daughter label; some N values also differ. | Generate count-based cohort tables with their actual Congress labels and sample sizes. | In Congress 105, **15.6** is the all-sons-to-all-daughters contrast; the per-daughter estimate is **7.74** points. Replacement tables are `tabs/si_washington_cohort.tex` and `tabs/si_other_cohort.tex`. |
| The balance table labeled t-test degrees of freedom as N. | Report the number of legislators and repair the grouped count used in the balance plot. | Each of the **10** displayed categories gains one in the N column. The table covers **1,464** legislators; the single legislator with 12 children remains excluded. Regression samples do not change. |
| Hardcoded paths, broken diagnostics and console-only results impeded reproduction. | Use project-relative R scripts, tidyverse analysis names, standard `broom` bootstrap summaries and `knitr` tables. Preserve existing figure styling; correct the Washington-period shading to start at Congress 105. | `make reproduce` rebuilds current outputs; `make validate` establishes historical and corrected numbers. Workflow cleanup reproduces all four corrected derived CSVs exactly. `renv.lock` records the validated dependencies. |

The PDFs in `ms/` remain historical: editable source is absent. Current numerical replacements are in `tabs/` and `figs/`. The manuscript prose has not been revised.

Interpretation remains for discussion. The pooled interval permits effects of several AAUW points; it does not establish a negligible effect. In the fully corrected Washington cohort, a joint test rejects equal annual slopes (p = .0010), while a linear trend is unclear (p = .82). These test different claims. Equal-legislator weighting gives **0.19** rather than **1.93** points in the isolated comparison using the historical data without the redundant join. That is an alternative estimand, not a coding correction. These comparisons use legislator-clustered inference; the main pooled results retain the manuscript's wild bootstrap.

Child timing is a separate, deferred robustness exercise. The released panel holds family composition fixed; dated births during service warrant investigation, but no time-varying specification has been substituted here. The SI already discusses fertility stopping, biological children, party adjustment and changing AAUW content.

Historical snapshots are preserved in Git:

- `manuscript-2023-01-30` (`ee7f1ca`): archived manuscript snapshot.
- `audit-baseline-2026-09-12` (`32437a5`): pre-edit code and data; all 20 main-table coefficients and sample sizes reproduce.
- `corrected-replication-v1.0.0`: the corrected code, data and numerical outputs. The historical PDFs and deferred interpretation/timing work are explicitly outside this release's corrections.

Run `make validate COMPARISONS=/tmp/daughters-comparisons` to establish these numbers from the historical Git tag and current inputs. It writes the comparisons and package versions outside the paper-output directories. The earlier investigation remains in Git history at `1d51917`; there is no separate audit dossier in this release.
