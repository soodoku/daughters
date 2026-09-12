# Changes from the manuscript version

The analytical files are still unchanged. The comparisons below were validated separately; the corresponding edits are pending. AAUW effects are in points on its 0–100 scale. References to the paper mean the local manuscript and supplement; the final journal PDFs have not yet been checked.

| Issue | Action / status | Consequence |
|---|---|---|
| “Any daughter” uses `ngirls > 1`. Both Costa et al. and our SI define at least one. | Pending: use `ngirls > 0` upstream. | Reclassifies 3,441 member–Congress rows. Supplementary pooled estimate **1.58 → 0.83**; both intervals include zero. Main daughter-count estimate unchanged. |
| An unnecessary Voteview join adds 14 rows. | Pending: remove it from AAUW models; resolve party-switch records explicitly where ideology scores are needed. | Main N **7,670 → 7,656**, still 1,459 legislators. Pooled estimate essentially unchanged. |
| Early scoring counts paired votes as votes and matches roll numbers across chambers. | Pending: follow the SI's paired-vote rule and match House votes only. | Changes 42 member–Congress scores. Together with removing the duplicate join, pooled estimate **1.929 → 1.927**. |
| Washington-cohort assignment includes 36 people without House service during Congresses 105–108. | Pending: require House service in that period. | Reassigns 128 analyzed rows. Cohort-slope gap **9.28 → 9.14**; the broad cohort difference survives. |
| SI 6.1 labels proportion-of-daughters coefficients as effects per daughter; some sample counts also differ. | Pending: regenerate the intended count-based table. | Example, Congress 105: **15.6** describes the all-sons-to-all-daughters contrast; the per-daughter coefficient is **7.74**. |
| SI describes five dated births, but released child counts never change within legislators. Diaz-Balart also has conflicting source records. | Unresolved: recover the case histories and adjudicate sources before recoding. | Numerical impact unknown. No guessed dates or automatic first/last-row repair. |
| The balance table reports t-test degrees of freedom as N. | Pending: report legislators counted. | Corrects descriptive counts; regression samples unchanged. |
| Hardcoded paths, broken exploratory diagnostics and obsolete README script links impede rerunning. | Pending: simplify the existing R workflow with standard libraries and project-relative paths. | A reproducible build; substantive estimates should change only for documented analytical edits. |

Interpretation remains for discussion. The pooled interval of approximately **−1.0 to +4.9** does not establish a negligible effect. A direct test rejects equal annual slopes within the corrected Washington cohort (**p = .00089**), while a linear trend remains unclear (**p = .81**). These test different claims. Equal-legislator weighting gives **0.19** rather than **1.93** points; it is an optional alternative estimand, not a coding correction. The existing SI already discusses fertility stopping, biological children, party adjustment and changing AAUW content.

Historical versions are preserved in Git:

- `manuscript-2023-01-30`: archived manuscript snapshot (`ee7f1ca`), without certification of final journal equivalence.
- `audit-baseline-2026-09-12`: pre-edit code and data (`32437a5`); all 20 main-table coefficients and sample sizes reproduce.
- `directors-cut-v1.0.0`: reserved for the completed, validated revision.

Run the numerical comparisons from the repository root with `Rscript --vanilla scripts/audit.R /tmp/daughters-comparisons`. The script records package versions and writes comparisons outside the paper-output directories. As edits land, update each row with what was done and its final consequence. The longer investigation remains in Git history at `1d51917`.
