### Balance Checks

### Load libs
library(stargazer)
library(tidyverse)
library(knitr)
library(broom)

# Set seed
set.seed(1234567)

# Balance Among All Number of Children

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


## Balance Tests

p <- d |>
  select(id, n_daughters, n_children) |>
  unique() |>
  group_by(n_children) |>
  summarize(
    prop_daughters = mean(n_daughters) / mean(n_children),
    n = n()
  ) |>
  ungroup() |>
  ggplot(aes(as.factor(n_children), prop_daughters, size = n)) +
  geom_point() +
  labs(
    x = "Total Children",
    y = "Proportion Daughters",
    title = "Proportion of Daughters by Total Children"
  ) +
  theme_minimal()

d_unique <- d |>
  group_by(id) |>
  sample_n(1) |>
  ungroup()

pre_ebonya_unique <- d |>
  filter(congress < 105) |>
  group_by(id) |>
  sample_n(1) |>
  ungroup()

ebonya_unique <- d |>
  filter(congress %in% c(105:108)) |>
  group_by(id) |>
  sample_n(1) |>
  ungroup()

post_ebonya_unique <- d |>
  filter(congress > 108) |>
  group_by(id) |>
  sample_n(1) |>
  ungroup()

## __All Unique MCs__ T-Test Proportion Girls

t.test(d_unique$prop_daughters, mu = 0.4878)

## __Pre-Ebonya__ T-Test Proportion Girls for All Unique MCs

t.test(pre_ebonya_unique$prop_daughters, mu = 0.4878)

## __During Ebonya__ T-Test Proportion Girls for All Unique MCs

t.test(ebonya_unique$prop_daughters, mu = 0.4878)

## __Post-Ebonya__ T-Test Proportion Girls for All Unique MCs

t.test(post_ebonya_unique$prop_daughters, mu = 0.4878)

# Conditional on Number of Children

## __All Unique MCs__ T-Test Conditional on N Children


t_all <- d_unique |>
  filter(n_children != 12) |> # not enough mc w/ 12 children
  group_by(n_children) |>
  summarise(n = n(), res = list(tidy(t.test(prop_daughters, mu = 0.4878)))) |>
  unnest(cols = res)

kable(t_all, digits = 3)

t_all$n_children <- as.integer(t_all$n_children)

t_show <- t_all |> select(n_children, estimate, p.value, n)
names(t_show) <- c("Number of Children", "Mean Proportion Daughters", "p", "n")

knitr::kable(
  t_show,
  format = "latex",
  booktabs = TRUE,
  digits = c(0, 3, 3, 0),
  caption = "Proportion of Female Children by Number of Children"
) |>
  kableExtra::footnote(
    general = paste(
      "Two-sided t-tests against 0.4878. N counts legislators.",
      "The single legislator with 12 children is omitted."
    )
  ) |>
  writeLines("tabs/append_prop_female_by_nchild.tex")

## __Pre-Ebonya__ T-Test Conditional on N Children

t_pre_ebonya <- pre_ebonya_unique |>
  filter(!n_children %in% c(9, 10)) |>
  group_by(n_children) |>
  summarise(n = n(), res = list(tidy(t.test(prop_daughters, mu = 0.4878)))) |>
  unnest(cols = res)

kable(t_pre_ebonya, digits = 3)


## __During-Ebonya__ T-Test Conditional on N Children

t_during_ebonya <- ebonya_unique |>
  filter(!n_children %in% c(10, 12)) |>
  group_by(n_children) |>
  summarise(n = n(), res = list(tidy(t.test(prop_daughters, mu = 0.4878)))) |>
  unnest(cols = res)

kable(t_during_ebonya, digits = 3)

## __Post-Ebonya__ T-Test Conditional on N Children

t_post_ebonya <- post_ebonya_unique |>
  filter(!n_children %in% c(12)) |>
  group_by(n_children) |>
  summarise(n = n(), res = list(tidy(t.test(prop_daughters, mu = 0.4878)))) |>
  unnest(cols = res)

kable(t_post_ebonya, digits = 3)

# Correlation between Proportion of Daughters and Number of Children

## All Unique MCs

cor(d_unique$prop_daughters, d_unique$n_children)

## __Pre-Ebonya__ All Unique MCs

cor(pre_ebonya_unique$prop_daughters, pre_ebonya_unique$n_children)

## __During-Ebonya__ All Unique MCs

cor(ebonya_unique$prop_daughters, ebonya_unique$n_children)

## __Post-Ebonya__ All Unique MCs

cor(post_ebonya_unique$prop_daughters, post_ebonya_unique$n_children)

# Regressing Proportion Daughters on Party

d_unique <- d_unique |>
  filter(party %in% c("D", "R")) |>
  mutate(democrat = ifelse(party == "D", 1, 0))

t_all_party <- d_unique |>
  # filter(n_children != 12) |> # not enough mc w/ 12 children
  group_by(democrat) |>
  summarise(n = n(), res = list(tidy(t.test(prop_daughters, mu = 0.4878)))) |>
  unnest(cols = c(res))

party_ngirls <- glm(
  democrat ~ n_daughters + as.factor(n_children),
  d_unique,
  family = "binomial"
)

stargazer(
  party_ngirls,
  covariate.labels = c("Number of Daughters", "Constant"),
  label = "tab:party_ngirls",
  dep.var.labels = "p(Democrat)",
  omit = "n_children",
  title = "Daughters and party, controlling for number of children",
  out = "tabs/party_ngirls.tex"
)


# Regressing Proportion Daughters on Covariates

## Region
# Region definitions: see the source link in the README.
regions <- read_csv("data/us_census_bureau_regions_and_divisions.csv")

d_unique_states <- d_unique |>
  left_join(
    regions |> select(`State Code`, Region),
    by = c("state" = "State Code")
  )

table(d_unique_states$Region)

summary(lm(
  n_daughters ~ as.factor(Region) + as.factor(n_children),
  d_unique_states
))


## Female Congress Members

### Number of Female Congress Members

table(d_unique$female)

summary(lm(prop_daughters ~ female + as.factor(n_children), d_unique))
