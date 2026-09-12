# Daughters: audit and revision plan

Audit date: September 12, 2026. Baseline: `32437a5cf19c0cfbb5937cba98aa6c5b934fcb88`.

The main numerical result survives the verified coding corrections. All 20 Congress-specific coefficients and sample sizes in the local manuscript's Table 1 reproduce. Removing an unnecessary join and correcting two early-vote construction errors changes the pooled estimate from **1.929 to 1.927 AAUW points per daughter**. These corrections do not overturn the paper.

The more consequential findings concern the supplementary daughter indicator, cohort membership and table labels, an unresolved discrepancy about when children were born, and the distinction between no monotonic decline and no temporal variation. Those warrant targeted revisions and discussion with the authors. This audit changes no analytical inputs, paper tables, figures, or manuscript language.

## Scope and reproduction

The comparison is against `ms/ms.pdf` and `ms/si.pdf`, committed January 30, 2023. The manuscript is byte-identical to the [author-hosted PDF](https://www.gsood.com/research/papers/daughters.pdf). The [journal version](https://doi.org/10.1086/724744) appeared in *Journal of Political Economy Microeconomics* 1(3), 506–516. The publisher's final article and supplement returned access errors. Findings about wording and table labels therefore refer to the local draft; they are not yet certified discrepancies in the final journal article.

The pipeline is source rosters and votes → scripts 01–03 → the frozen member–Congress CSV → scripts 04–09 → tables and figures → manually assembled manuscript PDFs. There are nine original R scripts. The frozen file has 8,270 rows and 1,465 legislators; 7,656 rows and 1,459 legislators enter the main outcome regression before the redundant join. Reconstruction of the upstream numerical data matched the frozen file. Manuscript source files and a dated child-history handoff were not found.

Run the audit from the repository root:

```sh
Rscript --vanilla scripts/audit.R /tmp/daughters-audit-results
```

It uses `dplyr`, `readr`, `tidyr`, `purrr`, `broom`, `dqrng`, `fwildclusterboot`, `lmtest`, `sandwich`, and `fixest`. Install missing packages with `install.packages()`. Tested with R 4.6.0 and `fwildclusterboot` 0.14.3. The script records package versions alongside the comparisons. Outputs go to the supplied directory, or a temporary directory by default. They are not another maintained set of paper tables.

The script reproduces historical and alternative estimates, identifies affected records, rebuilds early AAUW scores from raw vote codes, checks the 20 main coefficients and counts, and checks invariance to reversing row order. Wild-bootstrap comparisons use 9,999 draws and both standard and `dqrng` seeds, following the [package documentation](https://s3alfisc.github.io/fwildclusterboot/articles/fwildclusterboot.html). Small interval differences from historical software or bootstrap draws are not classified as corrections.

## What the central comparisons establish

| Claim and purpose | Quantity and design | Implementation | Interpretation |
|---|---|---|---|
| Estimate the average daughters association | AAUW score on daughter count, family-size indicators, legislator gender and Congress effects; each observed member–Congress gets one row; uncertainty clustered by legislator | Main Table 1 point estimates and counts reproduce; minor join/vote defects below | A causal reading needs sex composition to be effectively random conditional on family size among observed legislators. Fertility stopping, selection into office and biological-child measurement matter. The SI already discusses these concerns. |
| Test declining influence as polarization increases | Compare daughter slopes across Congresses and within the Washington-service cohort | Cohort assignment includes 36 people without House service in the defining period; direct temporal tests differ from visual interval comparisons | No clear linear trend is supported. Constancy over all Congresses is a different hypothesis and is rejected within the Washington cohort. |
| Show Washington's cohort differs from others | Compare slopes outside Congresses 105–108, where both cohorts are observable; permit cohort-specific controls and Congress effects | Corrected cohort gap remains about 9.1 AAUW points per daughter | This establishes a descriptive cohort difference. It does not distinguish chance selection of a striking study from persistent cohort differences, measurement, or selection into service. |
| Check alternative exposures and outcomes | Any daughter, daughter proportion, voting-only AAUW, women-specific votes, ideology and party | The any-daughter indicator is incorrect; SI 6.1 also mixes an exposure label with proportion estimates | These robustness exercises address different dimensions. Party is potentially post-treatment; adding it is a sensitivity choice, as the paper explains. They do not independently establish that the main causal assumptions hold. |

AAUW units below are points on its 0–100 scale. The source regressions use scores divided by 100.

## Verified numerical discrepancies

| Issue | Historical → corrected comparison | Implication and repair |
|---|---|---|
| **“Any daughter” means two or more.** Script 03 defines `anygirls` with `ngirls > 1`; SI 8.1 says at least one. Exactly one daughter is miscoded in 3,441 frozen rows. | Pooled indicator coefficient **1.58 → 0.83 points**, holding the historical regression sample fixed. Wild-bootstrap 95% intervals **[−3.91, 7.03] → [−5.68, 7.35]**. | Fix the indicator upstream to `ngirls > 0`; regenerate SI 8.1. This changes the contrast being estimated, but both estimates remain imprecise. The main count-of-daughters coefficient is unaffected by this fix alone. |
| **A redundant Voteview join repeats 14 member–Congress observations.** Within-Congress party switches produce multiple Voteview records. | Main N **7,670 → 7,656**; unique legislators stay **1,459**. Pooled slope **1.929 → 1.933**. | Remove the extra join from AAUW analyses, which do not need it. For NOMINATE analyses, specify a rule for within-term party-switch records instead of silently duplicating people. |
| **Paired votes are treated as actual votes.** The [Voteview codebook](https://voteview.com/articles/data_help_votes) identifies codes 2 and 5 as paired votes; SI 5.1 says to treat them as abstentions. | Correcting those codes changes **41** main AAUW scores in Congresses 97–101. On unique rows, the pooled slope is **1.933 → 1.930**. | Follow the documented rule at vote construction. Some individual scores move substantially, up to 25 points, although the pooled coefficient barely moves. This is separate from the defensible choice of whether abstentions belong in the score denominator. |
| **The early-vote join omits chamber.** Roll numbers are reused across House and Senate. | House-only matching changes one retained score: **Akaka, Congress 101, 89 → 100**. Pooled slope **1.933 → 1.930**, holding the paired-vote rule fixed. | Include chamber in the source match. Both vote fixes together alter **42** member–Congress scores and produce the **1.927** pooled coefficient on unique rows. |
| **Washington-cohort membership includes non-House service.** Assignment occurs before missing-outcome rows drop. | **569 → 533** cohort IDs in the frozen file; **128** analyzed rows belonging to **36** legislators move to the other cohort. On unique rows, the pooled cohort-slope gap changes **9.276 → 9.140** points. | Define membership from verified House service in Congresses 105–108. The gap remains substantial; early annual estimates and Figure 1 need regeneration. This fix alone does not change the full-sample pooled slope. |
| **SI 6.1 labels proportion coefficients as daughter-count effects.** | For Congresses 105, 108 and 116, printed coefficients are **15.6, 9.4 and 14.5 points**; count-based regressions give **7.74, 4.57 and 7.23**. The printed values reproduce from daughter proportion. | A proportion coefficient compares all sons with all daughters, conditional on family size; it is not the effect of one additional daughter. Rebuild this table from the intended model. Early sample counts also differ from current code, so relabeling alone is insufficient. The committed Figure 1 uses the count scale. |

These are isolated comparisons unless explicitly described as combined. The script saves annual estimates and affected IDs so that later edits can be checked against the same evidence.

A smaller table defect: script 07 calls the t-test degrees of freedom `n` in the balance table. It should report the number of legislators. This changes a descriptive count, not the regression sample.

## Important unresolved measurement issue

SI 5.2, p. 20, says that five legislators had children after Washington collected her data and that the children's ages were used to change coding between Congresses. In the released panel, **all 1,465 legislators have constant daughter and total-child counts**. Script 03 selects one child record per ID and applies it to every Congress.

This is a verified discrepancy between the described procedure and the released implementation. Its numerical consequence is **unknown**. The next step is to locate the five case histories and dates, establish which observed Congresses should differ, and then show the effect of using dated counts. Do not assume that every static count is wrong or invent birth dates. The SI already acknowledges difficulties measuring family composition.

There is also one conflicting duplicate in the child master: Mario Diaz-Balart (`D000600`) appears with zero children and, later, two sons. The first row wins and excludes him throughout Congresses 108–116. Other supplied sources indicate one son. Neither first-wins nor last-wins resolves this. The January 2023 commit labeled “fix diaz balart” changes the separate non-biological-child file; it does not resolve this master-file conflict. Establish the child's birth timing and biological status before changing eligibility.

Two other records have daughter-plus-son counts inconsistent with their totals: Donnelly and Canseco, affecting four main-outcome rows. Primary sources support their existing totals: [Donnelly's Congressional Record statement](https://www.congress.gov/110/crec/2007/01/09/CREC-2007-01-09-pt1-PgH232-2.pdf) and [Canseco's House biography](https://history.house.gov/People/Detail/11824). Do not mechanically replace totals with the sums. A correction confined to the unused son counts would leave the main regressions unchanged.

## Interpretation and analytical choices to discuss

1. **No clear average effect is more defensible than a negligible effect.** The manuscript's pooled 95% interval spans approximately **−1.0 to +4.9 AAUW points per daughter**. Its upper endpoint is about 88% of the **5.6-point** estimate for Washington's period. The point estimate is small relative to the paper's party gap, but the interval still permits a material daughters association. Deciding what counts as substantively small requires an explicit benchmark.

2. **No monotonic decline does not imply no temporal variation.** With verified House-cohort membership, a legislator-clustered joint test of equal annual daughter slopes gives **F(19, 532) = 2.385, p = .00089**. A linear trend test gives **p = .81**. The first challenges the local draft's broad chance-variation sentence; the second supports its narrower argument against a monotonic decline. The joint model allows annual family-size and gender coefficients to vary. These are additional diagnostic tests, not discoveries of incorrect original p-values; the manuscript does not report this joint test. Sparse family-size-by-Congress cells and the unbalanced cohort warrant keeping the inference qualified.

3. **An equal-legislator analysis is a different estimand.** Giving each legislator total weight one across their observed complete-case Congresses changes the pooled coefficient from **1.93 to 0.19 points**; clustered 95% intervals are **[−0.91, 4.78]** and **[−2.07, 2.45]**. This strengthens the weak-average description but also shows that longer-serving legislators matter to the pooled result. Retain member–Congress weighting as the manuscript's primary specification and present equal-legislator weighting as a limited sensitivity if desired.

4. **A cohort pattern is not a test of the publication-selection mechanism.** The cohort gap survives correction, which is useful evidence. It does not establish why that gap arose. The paper already presents file-drawer selection as a possible explanation; preserve that qualification. Selection into elected office cannot be dismissed solely because the outcome coefficient is insignificant. Sex composition is fixed in the released panel, so legislator fixed effects would absorb the main exposure rather than solve identification.

## Checks, rejected candidates and limits

| Audit area | Result |
|---|---|
| Tier 1: identity, coding, joins, denominators, source-to-output consistency | Unique frozen member–Congress keys; 14 rows added downstream; indicator and early-vote discrepancies verified; Table 1 coefficients and Ns reproduce. |
| Tier 2: distributions and missingness | Scores remain in bounds. The 614 missing main outcomes comprise 577 missing-chamber roster rows and 37 House rows. Of the 577, 550 match Senate service and 27 remain unmatched. Missing outcomes remain missing rather than zero. Differential missingness alone does not establish bias. |
| Tier 2: construction, estimands, inference and skew | Raw early scores reconstructed; alternate exposure labels, weighting and clustered inference examined. Identifier skew has no substantive meaning. The mechanical leave-one-row screen uses a simplified model and is not a legislator-level influence audit. Full leave-one-legislator-out sensitivity remains an optional extension. |
| Tier 3: design, measurement, selection and theory | Biological-child restriction, stopping rules, political selection and changing AAUW content read against existing SI discussion. No claim that these were ignored. Birth timing needs source adjudication. An assigned intervention, compliance analysis and experimental attrition bounds are inapplicable. |
| Tier 4: econometric design | This is a conditional natural-experiment argument with repeated outcomes, not a DiD, RD or IV design. Legislator clustering addresses repeated legislators; it does not validate random assignment or fix post-treatment selection. Period/cohort contrasts estimated directly. |
| Paper checks: full local manuscript/SI, units, comparisons, evidence and rival explanations | Main text and SI read; core results reproduced; SI coefficient units checked; committed Figure 1 inspected. Final journal wording, manuscript compilation and exhaustive verification of the literature-review table remain unverified. No registration or prespecified analysis plan found. |
| Executability | Scripts 04, 05, 06 and 09 completed locally; script 09 required bypassing its hardcoded working directory. Script 07 fails at `summarize(n = n)` with current dplyr. Script 08 reaches the main table, then fails in an added fractional-bootstrap diagnostic with a weights-length error. The separate audit script completes and passes lint. |

Rejected numerical criticisms: the frozen final data are not demonstrably stale; reconstruction matches their numerical contents. Penalizing abstentions, including final cosponsors, omitting party from the primary model and using complete cases are documented choices, not automatically coding errors. An unused ID fallback defect affects no supplied observations. Small bootstrap and rounding differences are not substantive corrections. The inconsistent son counts do not justify changing source-supported child totals.

The original project has no dependency lock, contains hardcoded working directories, and has README links to obsolete script names. Several results are printed for manual copying rather than saved to a manuscript build. Those are reproducibility improvements to make during revision, without adding production-style CI. The audit script uses standard inference libraries and leaves historical files untouched.

## Proposed revision and release sequence

1. **Preserve the evidence.** Annotated tag `manuscript-2023-01-30` identifies `ee7f1caa4721085c2bc5a954aaf4afe86388acaa`; `audit-baseline-2026-09-12` identifies `32437a5cf19c0cfbb5937cba98aa6c5b934fcb88`. The first means the archived manuscript snapshot, not a guarantee that every artifact equals the final journal version. The second reproduces Table 1's main coefficients and counts.
2. **Resolve the narrow source questions.** Locate the five dated births and adjudicate Diaz-Balart. Confirm the final journal text and SI. Keep an explicit unresolved status if those artifacts cannot be recovered; do not substitute guessed corrections.
3. **Repair definitions upstream.** Correct any-daughter coding, House-vote matching, paired votes, the redundant analysis join and House-cohort membership. Verify row conservation, source-vote keys, daughter-indicator identities and old/new coefficients. Handle NOMINATE party-switch records with a documented rule. Rebuild the intended SI 6.1 model and descriptive counts.
4. **Agree on interpretation and a short sensitivity set.** Retain the manuscript's main specification; show the direct cohort/time tests and, if wanted, equal-legislator weighting. Review the changes to “no appreciable effect” and the broad chance-variation statement with the authors before rewriting them. Avoid an open-ended specification search.
5. **Make the R workflow easy to rerun.** Use project-relative paths, tidyverse naming, standard model and table packages, one documented command and pinned dependencies. Remove broken exploratory diagnostics from the paper build. Fix README links. Regenerate affected existing tables and figures without redesigning their style; update only the manuscript numbers and claims they feed once sources are available.
6. **Release the director's cut after validation.** Tag the selected, checked revision `directors-cut-v1.0.0`, accompanied by a concise account of numerical changes and chosen alternatives. Keep historical tags immutable. No parallel `clean` data/tables tree and no director's-cut tag on unfinished revisions.
