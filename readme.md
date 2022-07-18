### Replication Materials For "Revisiting a Natural Experiment: Do Legislators With Daughters Vote More Liberally on Women's Issues?"

An intriguing natural experiment arises from the fact that legislators are randomly assigned some combination of sons or daughters. The pioneering work of Washington (2008) shows that legislators with daughters cast more liberal roll call votes on women's issues. Costa et al. (2019) find that this pattern subsides in more recent congresses and speculate that increasing party polarization might diminish the ``daughters effect.'' The present paper delves more deeply into patterns of change over time by looking at eight congresses prior to the four studied by Washington (2008) as well as eight subsequent congresses, including three not included in Costa et al. (2019). Contrary to the party polarization hypothesis, we find no daughters effect leading up to the period that Washington studied and no effect thereafter. The cohort of members whom Washington studied exhibit consistently positive effects over time, while other legislators exhibit non-positive effects.The daughters effect appears to be a statistical aberration.

### Manuscript

* [Manuscript](ms/ms.pdf)
* [Supporting Information](ms/si.pdf)

### Data

1. [AAUW Data 97th--116th Congresses](https://doi.org/10.7910/DVN/HD5VHI)
2. [Costa et al. (2019) Replication Archive](data/costa_et_al/)
3. [Washington (2008) Replication Archive](data/washington)
4. [Congressional Member ID Data](data/member_id/)
5. [Voteview Data](data/voteview_congress_members.csv)
6. [Female Members of Congress](data/female members%20of%20congress.csv)
7. [Data on Children of MCs](data/Child%20Info%20Master%20List%20Dotters.csv)
8. [US Census Bureau Regions and Divisions](data/us_census_bureau_regions_and_divisions.csv)
	via https://raw.githubusercontent.com/cphalpert/census-regions/master/us%20census%20bureau%20regions%20and%20divisions.csv
9. [Literature Review](data/dotters_lit.csv)

### Scripts

1. [Official Congress List](scripts/official_cong_list.R) takes Congressional Member ID data and the Voteview data to produce [Official Congressinal List Data](data/official_cong_list.csv)

2. [AAUW Vote Data](scripts/aauw_full.R) takes the data from #1 and AAUW vote data to produce two intermediate files [aauw vote data](data/aauw_scores_long.csv) and [aauw women's only votes data](data/aauw_womens.csv)

3. [Final Data Creation](scripts/final_dataset_wrangle.R) takes the data from #1, merges with data on children of mcs, female members of congress data, output from #2, etc. to produce the [final data](data/final_data_2022_01_05.csv) we use for analysis.

4. [Balance Checks](scripts/balance_checks.R)
	* Produces results in SI 4 including [Table SI 4.1: Proportion of Female Children by Number of Children](tabs/append_prop_female_by_nchild.tex)

5. [Replicating Costa et al. (2019)](tabs/06_costa_et_al_rep.R)
	* First Six Columns of Table SI 3.2: Costa et al. (2019) Replication

6. [Replicating Washington (2008)](tabs/07_washington_rep.R)
	* First Five Columns of Table SI 3.1: Washington (2008) Replication

7. [Main Text Table and Figure](scripts/08_daughters_paper_outputs.R)
	* [Figure 1: Effect of Daughters Over Time By Cohort Analyzed by Washington (2008) and All Others](figs/fig_1_ebonya_cohort.pdf)
	* [Table 1: Effect of Daughters on AAUW by Congress](tabs/table_1_ngirls_aauw_by_cong.tex)

8. [09_daughters_fulldata_analysis.R](scripts/09_daughters_fulldata_analysis.R)
	* Produces [Figure SI 7.1: AAUW Scores by Party Over Time](figs/si_aauw_over_time.pdf)

9. [Compiling Literature Review Into a Latex Table](scripts/lit_review.R)
	* Produces [Table SI 1.1: Literature Review](tabs/appendix_lit_review.tex)

### Authors

[Donald Green](http://donaldgreen.com/), [Oliver Hyman-Metzger](https://github.com/olivermetzger), [Gaurav Sood](https://github.com/soodoku), and [Michelle A. Zee](https://github.com/michelleazee)
