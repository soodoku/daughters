## Data Analysis
## Table 1: Effect of ndaughters over time
## Figure 1: Effect of ndaughters over time among Washington Cohort/Rest


### Load libs
library(stargazer)
library(tidyverse)
library(fwildclusterboot)
library(knitr)
library(lme4)

# Set seed
set.seed(1234567)

# Load data
d <- read_csv("data/final_data_2022_01_05.csv") |>
  rename(
    n_daughters = ngirls,
    n_children = nchildren,
    has_daughter = anygirls
  ) |>
  mutate(
    aauw_all = aauw_all / 100,
    aauw_women_all = aauw_women_all / 100,
    nominate_dim1 = -nominate_dim1,
    prop_daughters = n_daughters / n_children
  )

## All MCs/Table 1
## AAUW All (including cosponsorships), Ngirls

congress <- as.character(c(97:116))

fitted_ngirls_all <- d |>
  nest(.by = c(congress)) |>
  rowwise() |>
  mutate(
    aauw_model = list(lm(
      aauw_all ~ n_daughters + as.factor(n_children) + female,
      data = data
    ))
  ) |>
  mutate(
    co_aauw = coef(aauw_model)["n_daughters"],
    se_aauw = coef(summary(aauw_model))["n_daughters", "Std. Error"]
  )

stargazer(
  fitted_ngirls_all$aauw_model,
  column.labels = congress,
  covariate.labels = "N. Daughters",
  dep.var.labels = "AAUW",
  omit = c("n_children", "female", "Constant"),
  header = FALSE,
  type = "latex",
  omit.stat = c("LL", "ser", "f", "rsq"),
  float.env = "sidewaystable",
  font.size = "tiny",
  out = "tabs/table_1_ngirls_aauw_by_cong.tex",
  title = "Estimated Average Treatment Effect Among All MCs"
)

### Add party
fitted_ngirls_all_party <- d |>
  nest(.by = c(congress)) |>
  rowwise() |>
  mutate(
    aauw_model = list(lm(
      aauw_all ~ n_daughters + as.factor(n_children) + party + female,
      data = data
    ))
  ) |>
  mutate(
    co_aauw = coef(aauw_model)["n_daughters"],
    se_aauw = coef(summary(aauw_model))["n_daughters", "Std. Error"]
  )

stargazer(
  fitted_ngirls_all_party$aauw_model,
  column.labels = congress,
  covariate.labels = "N. Daughters",
  dep.var.labels = "AAUW",
  omit = c("n_children", "female", "Constant"),
  header = FALSE,
  type = "latex",
  omit.stat = c("LL", "ser", "f", "rsq"),
  float.env = "sidewaystable",
  font.size = "tiny",
  out = "tabs/tab_1_pid.tex",
  title = "Effect per daughter, adjusting for family size, gender and party"
)

### Pooled Reg
aauw_ngirls <- lm(
  aauw_all ~ n_daughters + as.factor(congress) + as.factor(n_children) + female,
  d
)
set.seed(1234567)
dqrng::dqset.seed(1234567)
boot_aauw_ngirls <- boottest(
  aauw_ngirls,
  clustid = "id",
  param = "n_daughters",
  B = 9999,
  nthreads = 1
)

boot_aauw <- broom::tidy(boot_aauw_ngirls)

### Pooled with Party
aauw_ngirls_party <- lm(
  aauw_all ~ n_daughters +
    as.factor(congress) +
    party +
    as.factor(n_children) +
    female,
  d
)
set.seed(1234567)
dqrng::dqset.seed(1234567)
boot_aauw_ngirls_party <- boottest(
  aauw_ngirls_party,
  clustid = "id",
  param = "n_daughters",
  B = 9999,
  nthreads = 1
)
boot_aauw_party <- broom::tidy(boot_aauw_ngirls_party)

### Hierarchical Model/Pooled
aauw_ngirls_hier <- lmer(
  aauw_all ~ n_daughters +
    as.factor(congress) +
    as.factor(n_children) +
    female +
    (1 | id),
  d
)

stargazer(
  aauw_ngirls_hier,
  covariate.labels = "N. Daughters",
  dep.var.labels = "AAUW",
  omit = c("n_children", "female", "congress", "Constant"),
  header = FALSE,
  type = "latex",
  omit.stat = c("LL", "ser", "f", "rsq"),
  out = "tabs/table_1a_ngirls_aauw_pooled_hier.tex",
  title = "Effect per daughter: hierarchical model"
)

## EB's cohort vs. rest./Figure 1

washington_cohort <- d |>
  filter(congress %in% 105:108, chamber == "House") |>
  distinct(id) |>
  pull(id)

d <- d |>
  mutate(
    washington_cohort = ifelse(
      id %in% washington_cohort,
      "Washington",
      "Non-Washington"
    )
  )

fitted_ngirls <- d |>
  drop_na(aauw_all, n_daughters, n_children, female) |>
  nest(.by = c(congress, washington_cohort)) |>
  rowwise() |>
  mutate(
    aauw_model = list(lm(
      aauw_all ~ n_daughters + as.factor(n_children) + female,
      data = data
    ))
  ) |>
  mutate(
    co_aauw = coef(aauw_model)["n_daughters"],
    se_aauw = coef(summary(aauw_model))["n_daughters", "Std. Error"]
  )

fitted_ngirls |>
  ungroup() |>
  select(congress, washington_cohort, co_aauw, se_aauw) |>
  pivot_longer(
    cols = c(co_aauw:se_aauw),
    names_to = c("type", "dep_var"),
    names_sep = "_"
  ) |>
  pivot_wider(names_from = type) |>
  ggplot(aes(congress, co)) +
  annotate(
    "rect",
    xmin = 105,
    xmax = 108,
    ymin = -Inf,
    ymax = Inf,
    fill = "#eeeeee"
  ) +
  annotate(
    "rect",
    xmin = 110,
    xmax = 114,
    ymin = -Inf,
    ymax = Inf,
    fill = "#eeeeee"
  ) +
  annotate("text", x = 106.5, y = 0.3, label = "Washington") +
  annotate("text", x = 112, y = 0.3, label = "Costa et al.") +
  geom_line(aes(color = (washington_cohort), linetype = washington_cohort)) +
  scale_color_manual(values = c("#777777", "black")) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  geom_pointrange(
    aes(
      ymin = co - 1.96 * se,
      ymax = co + 1.96 * se,
      color = as.factor(washington_cohort)
    ),
    position = position_dodge(0.05)
  ) +
  labs(
    title = "Estimated Average Treatment Effect of Number of Daughters",
    x = "Congress",
    y = "AAUW",
    color = "Cohort",
    linetype = "Cohort"
  ) +
  theme_bw() +
  theme(legend.position = "bottom", legend.box = "vertical")

ggsave("figs/fig_1_ebonya_cohort.pdf")
ggsave("figs/fig_1_ebonya_cohort.eps")


## Washington cohort: effect per daughter

washington_models <- filter(fitted_ngirls, washington_cohort == "Washington")
stargazer(
  washington_models$aauw_model,
  column.labels = as.character(washington_models$congress),
  covariate.labels = "N. Daughters",
  dep.var.labels = "AAUW",
  omit = c("n_children", "female", "Constant"),
  header = FALSE,
  type = "latex",
  omit.stat = c("LL", "ser", "f", "rsq"),
  float.env = "sidewaystable",
  out = "tabs/si_washington_cohort.tex",
  title = "Effect per daughter among the Washington cohort"
)

## Other legislators: effect per daughter

other_models <- filter(fitted_ngirls, washington_cohort == "Non-Washington")
stargazer(
  other_models$aauw_model,
  column.labels = as.character(other_models$congress),
  covariate.labels = "N. Daughters",
  dep.var.labels = "AAUW",
  omit = c("n_children", "female", "Constant"),
  header = FALSE,
  type = "latex",
  omit.stat = c("LL", "ser", "f", "rsq"),
  out = "tabs/si_other_cohort.tex",
  title = "Effect per daughter among other legislators"
)

## Pooled models by period

washington_period <- d$congress %in% c(105:108)

d <- d |>
  mutate(
    washington_period = ifelse(
      congress %in% c(105:108),
      "Washington Congress",
      "Non-Washington Congress"
    )
  )

nw_cong <- lm(
  aauw_all ~ n_daughters + as.factor(n_children) + female + as.factor(congress),
  data = d[d$washington_period == "Non-Washington Congress", ]
)
w_cong <- lm(
  aauw_all ~ n_daughters + as.factor(n_children) + female + as.factor(congress),
  data = d[d$washington_period == "Washington Congress", ]
)

set.seed(1234567)

dqrng::dqset.seed(1234567)

boot_nw_cong <- boottest(
  nw_cong,
  clustid = "id",
  param = "n_daughters",
  B = 9999,
  nthreads = 1
)
set.seed(1234567)
dqrng::dqset.seed(1234567)
boot_w_cong <- boottest(
  w_cong,
  clustid = "id",
  param = "n_daughters",
  B = 9999,
  nthreads = 1
)

## See the effect of including non-biological children

non_bio <- read.csv("data/children_with_non_biological_washington_cohort.csv")
non_bio <- non_bio[!duplicated(non_bio), ]
non_bio <- non_bio[non_bio$id != "", ]

non_bio <- non_bio[!duplicated(non_bio$icpsr), ]

# Merge with washington
ew_cong <- d[d$congress %in% c(105:108), ]

ew_cong_m <- ew_cong |>
  left_join(
    non_bio[, c("icpsr", "ngirls_total", "nchildren_total")],
    by = "icpsr",
    relationship = "many-to-one"
  )

w_cong_bio <- lm(
  aauw_all ~ n_daughters + as.factor(n_children) + female + as.factor(congress),
  data = ew_cong_m
)
set.seed(1234567)
dqrng::dqset.seed(1234567)
boot_w_cong_bio <- boottest(
  w_cong_bio,
  clustid = "id",
  param = "n_daughters",
  B = 9999,
  nthreads = 1
)

w_cong_non_bio <- lm(
  aauw_all ~ ngirls_total +
    as.factor(nchildren_total) +
    female +
    as.factor(congress),
  data = ew_cong_m
)
set.seed(1234567)
dqrng::dqset.seed(1234567)
boot_w_cong_non_bio <- boottest(
  w_cong_non_bio,
  clustid = "id",
  param = "ngirls_total",
  B = 9999,
  nthreads = 1
)

# Models including non-biological children
non_bio_105 <- lm(
  aauw_all ~ ngirls_total + as.factor(nchildren_total) + female,
  data = ew_cong_m[ew_cong_m$congress %in% 105, ]
)
non_bio_106 <- lm(
  aauw_all ~ ngirls_total + as.factor(nchildren_total) + female,
  data = ew_cong_m[ew_cong_m$congress %in% 106, ]
)
non_bio_107 <- lm(
  aauw_all ~ ngirls_total + as.factor(nchildren_total) + female,
  data = ew_cong_m[ew_cong_m$congress %in% 107, ]
)
non_bio_108 <- lm(
  aauw_all ~ ngirls_total + as.factor(nchildren_total) + female,
  data = ew_cong_m[ew_cong_m$congress %in% 108, ]
)

nonbiological_models <- list(
  non_bio_105, non_bio_106, non_bio_107, non_bio_108, w_cong_non_bio
)
stargazer(
  nonbiological_models,
  column.labels = c(105:108, "Pooled"),
  covariate.labels = "N. Daughters",
  dep.var.labels = "AAUW",
  omit = c("n_children", "congress", "female", "Constant"),
  header = FALSE,
  type = "latex",
  omit.stat = c("LL", "ser", "f", "rsq"),
  font.size = "small",
  out = "tabs/table_si_nonbio_ngirls_aauw_by_cong_washington.tex",
  title = "Effect per daughter including non-biological children"
)

pooled_bootstraps <- list(
  all = boot_aauw_ngirls,
  party_adjusted = boot_aauw_ngirls_party,
  outside_washington_period = boot_nw_cong,
  washington_period = boot_w_cong,
  biological = boot_w_cong_bio,
  including_nonbiological = boot_w_cong_non_bio
)
pooled_results <- imap_dfr(
  pooled_bootstraps,
  ~ broom::tidy(.x) |>
    mutate(model = .y, n = nobs(.x))
) |>
  select(model, everything())
write_csv(pooled_results, "tabs/pooled_daughters.csv")
knitr::kable(
  pooled_results,
  format = "latex",
  booktabs = TRUE,
  digits = 4,
  caption = "Pooled daughter-count models: legislator-clustered wild bootstrap"
) |>
  writeLines("tabs/pooled_daughters.tex")

annual_results <- bind_rows(
  mutate(fitted_ngirls_all, washington_cohort = "All"),
  fitted_ngirls
) |>
  rowwise() |>
  mutate(n = nobs(aauw_model)) |>
  ungroup() |>
  select(
    congress, washington_cohort, estimate = co_aauw, std_error = se_aauw, n
  )
write_csv(annual_results, "tabs/annual_daughters.csv")
stopifnot(
  all(is.finite(pooled_results$conf.low)),
  all(is.finite(pooled_results$conf.high))
)
