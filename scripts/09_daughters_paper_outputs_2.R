## Data Analysis
# 1. Pooled Regression
# 2.

### Set working dir

### Load libs
library(stargazer)
library(tidyverse)
library(fwildclusterboot)
library(knitr)

# Set seed
set.seed(1234567)

# Load dat

# AAUW is divided by 100; larger values indicate greater AAUW support.
# NOMINATE is reversed and rescaled to 0--1; larger values are more liberal.

d <- read_csv("data/final_data_2022_01_05.csv") |>
  rename(
    n_daughters = ngirls,
    n_children = nchildren,
    has_daughter = anygirls
  ) |>
  mutate(
    aauw_all = aauw_all / 100,
    aauw_women_all = aauw_women_all / 100,
    nom_reverse = -nominate_dim1,
    nominate_dim1 = (nom_reverse - min(nom_reverse, na.rm = TRUE)) /
      (max(nom_reverse, na.rm = TRUE) - min(nom_reverse, na.rm = TRUE)),
    prop_daughters = n_daughters / n_children
  )

## SI 7
## AAUW Over Time

d |>
  filter(party != "I") |> # too few I to use geom_smooth
  mutate(party = ifelse(party == "D", "Democratic", "Republican")) |>
  ggplot(aes(congress, aauw_all, color = party)) +
  geom_point(position = "jitter", alpha = 0.3, size = 0.7) +
  geom_smooth() +
  scale_color_manual(values = c("Democratic" = "blue", "Republican" = "red")) +
  theme_minimal() +
  labs(title = "AAUW by Congress", x = "Congress", y = "AAUW", color = "")

ggsave("figs/si_aauw_over_time.pdf")

# Supplementary exposures and outcomes. All outcomes use a 0--1 scale.
specifications <- tribble(
  ~outcome, ~exposure,
  "aauw_all", "has_daughter",
  "aauw_women_all", "n_daughters",
  "nominate_dim1", "n_daughters"
)
pooled_results <- list()
for (i in seq_len(nrow(specifications))) {
  outcome <- specifications$outcome[i]
  exposure <- specifications$exposure[i]
  formula <- reformulate(
    c(exposure, "factor(n_children)", "female", "factor(congress)"),
    response = outcome
  )
  model <- lm(formula, data = d)
  set.seed(1234567)
  dqrng::dqset.seed(1234567)
  boot <- boottest(
    model,
    clustid = "id",
    param = exposure,
    B = 9999,
    nthreads = 1
  )
  pooled_results[[i]] <- broom::tidy(boot) |>
    mutate(outcome, exposure, n = nobs(model))
}
stopifnot(all(map_lgl(
  pooled_results,
  ~ all(is.finite(.x$conf.low)) & all(is.finite(.x$conf.high))
)))
pooled_results <- bind_rows(pooled_results) |>
  select(outcome, exposure, everything())
write_csv(pooled_results, "tabs/pooled_supplement.csv")
knitr::kable(
  pooled_results,
  format = "latex",
  booktabs = TRUE,
  digits = 4,
  caption = "Supplementary models: legislator-clustered wild bootstrap"
) |>
  writeLines("tabs/pooled_supplement.tex")

# Annual OLS tables retain the manuscript's conventional standard errors.
annual_specifications <- tribble(
  ~outcome, ~exposure, ~file, ~label,
  "aauw_all", "has_daughter", "si_any_daughter", "Any daughter",
  "aauw_women_all", "n_daughters", "womens_issues_aauw", "N. Daughters",
  "nominate_dim1", "n_daughters", "si_nominate", "N. Daughters"
)
annual_results <- list()
for (i in seq_len(nrow(annual_specifications))) {
  outcome <- annual_specifications$outcome[i]
  exposure <- annual_specifications$exposure[i]
  formula <- reformulate(
    c(exposure, "factor(n_children)", "female"),
    response = outcome
  )
  fitted <- d |>
    nest(.by = congress) |>
    arrange(congress) |>
    mutate(model = map(data, ~ lm(formula, data = .x)))
  stargazer(
    fitted$model,
    column.labels = as.character(fitted$congress),
    covariate.labels = annual_specifications$label[i],
    omit = c("n_children", "female", "Constant"),
    header = FALSE,
    type = "latex",
    omit.stat = c("ll", "ser", "f"),
    out = paste0("tabs/", annual_specifications$file[i], ".tex")
  )
  annual_results[[i]] <- fitted |>
    transmute(
      congress,
      result = map(
        model,
        ~ broom::tidy(.x) |>
          filter(term == exposure) |>
          mutate(n = nobs(.x))
      )
    ) |>
    unnest(result) |>
    mutate(outcome, exposure)
}
write_csv(bind_rows(annual_results), "tabs/annual_supplement.csv")
