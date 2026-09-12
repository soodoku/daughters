library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(broom)

# Audit outputs are comparisons; the historical analytical inputs stay fixed.
args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args)) {
  args[[1]]
} else {
  file.path(tempdir(), "daughters-audit")
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
cat("Audit outputs:", normalizePath(output_dir), "\n")
frozen <- read_csv("data/final_data_2022_01_05.csv", show_col_types = FALSE) |>
  mutate(
    aauw_all = aauw_all / 100, aauw_voting = aauw_voting / 100,
    aauw_women_all = aauw_women_all / 100, prop_girls = ngirls / nchildren
  )
voteview <- read_csv(
  "data/voteview_congress_members.csv",
  show_col_types = FALSE
) |>
  filter(chamber == "House") |>
  select(id = bioguide_id, congress, nokken_poole_dim1)
historical <- left_join(frozen, voteview,
  by = c("id", "congress"),
  relationship = "many-to-many"
)
stopifnot(
  !anyDuplicated(frozen[c("id", "congress")]),
  all(frozen$nchildren > 0), nrow(historical) == nrow(frozen) + 14
)
write_csv(
  filter(frozen, ngirls + nboys != nchildren),
  file.path(output_dir, "family_count_conflicts.csv")
)
write_csv(
  voteview |> count(id, congress) |> filter(n > 1),
  file.path(output_dir, "duplicate_join_keys.csv")
)

scenarios <- list(
  historical = historical, join_removed = frozen,
  any_daughter_corrected = mutate(historical, anygirls = as.integer(ngirls > 0))
)
results <- list()
for (scenario in names(scenarios)) {
  for (exposure in c("ngirls", "anygirls", "prop_girls")) {
    analysis_data <- scenarios[[scenario]] |>
      drop_na(aauw_all, all_of(exposure), nchildren, female, congress, id)
    formula <- reformulate(
      c(exposure, "factor(nchildren)", "female", "factor(congress)"),
      response = "aauw_all"
    )
    model <- lm(formula, data = analysis_data)
    set.seed(1234567)
    dqrng::dqset.seed(1234567)
    boot <- fwildclusterboot::boottest(model,
      clustid = "id", param = exposure,
      B = 9999, nthreads = 1
    )
    results[[paste(scenario, exposure)]] <- tibble(
      scenario, exposure,
      estimate = unname(coef(model)[exposure]),
      lower = boot$conf_int[1], upper = boot$conf_int[2], p = boot$p_val,
      rows = nobs(model), legislators = n_distinct(analysis_data$id)
    )
  }
}
write_csv(bind_rows(results), file.path(output_dir, "pooled_comparisons.csv"))
print(bind_rows(results), n = Inf)

by_congress <- historical |>
  group_by(congress) |>
  group_modify(\(data, key) {
    map_dfr(c("ngirls", "anygirls", "prop_girls"), \(exposure) {
      formula <- reformulate(
        c(exposure, "factor(nchildren)", "female"),
        response = "aauw_all"
      )
      model <- lm(formula, data = data)
      tidy(model) |>
        filter(term == exposure) |>
        mutate(rows = nobs(model), exposure)
    })
  }) |>
  ungroup()
write_csv(by_congress, file.path(output_dir, "congress_reproduction.csv"))
expected_b <- c(
  -.006, -.004, -.022, .002, .002, .010, .021, .033, .077, .040,
  .068, .046, .019, .046, .002, .041, .030, .006, -.004, -.010
)
expected_n <- c(
  362, 366, 361, 361, 364, 368, 377, 386, 397, 393,
  392, 392, 390, 400, 392, 400, 401, 396, 395, 377
)
main <- filter(by_congress, exposure == "ngirls")
stopifnot(
  identical(round(main$estimate, 3), expected_b), all(main$rows == expected_n)
)

cohort_ids <- historical |>
  filter(between(congress, 105, 108)) |>
  pull(id) |>
  unique()
cohort_data <- historical |>
  mutate(washington_cohort = id %in% cohort_ids)
cohort_results <- cohort_data |>
  group_by(congress, washington_cohort) |>
  group_modify(\(data, key) {
    map_dfr(c("ngirls", "prop_girls"), \(exposure) {
      formula <- reformulate(
        c(exposure, "factor(nchildren)", "female"),
        response = "aauw_all"
      )
      model <- lm(formula, data = data)
      tidy(model) |>
        filter(term == exposure) |>
        mutate(rows = nobs(model), exposure)
    })
  }) |>
  ungroup()
write_csv(cohort_results, file.path(output_dir, "cohort_reproduction.csv"))

inference <- list()
for (period in c("pre", "washington", "post", "costa", "all")) {
  analysis_data <- frozen |>
    filter(switch(period,
      pre = congress < 105,
      washington = between(congress, 105, 108),
      post = congress > 108,
      costa = between(congress, 110, 114),
      all = TRUE
    )) |>
    drop_na(aauw_all, ngirls, nchildren, female, congress, id)
  model <- lm(
    aauw_all ~ ngirls + factor(nchildren) + female + factor(congress),
    data = analysis_data
  )
  clustered <- lmtest::coeftest(model,
    vcov. = sandwich::vcovCL(model, cluster = analysis_data$id, type = "HC1"),
    df = n_distinct(analysis_data$id) - 1
  )
  inference[[period]] <- tidy(clustered, conf.int = TRUE) |>
    filter(term == "ngirls") |>
    mutate(
      period,
      rows = nobs(model), legislators = n_distinct(analysis_data$id)
    )
}
write_csv(bind_rows(inference), file.path(output_dir, "period_estimates.csv"))

analysis_data <- frozen |>
  drop_na(aauw_all, ngirls, nchildren, female, congress, id) |>
  add_count(id, name = "terms_observed") |>
  mutate(
    washington_cohort = as.integer(id %in% cohort_ids),
    after_washington = as.integer(congress > 108),
    washington_period = as.integer(between(congress, 105, 108))
  )
member_congress <- fixest::feols(
  aauw_all ~ ngirls + factor(nchildren) + female | congress,
  data = analysis_data, vcov = ~id
)
equal_legislator <- fixest::feols(
  aauw_all ~ ngirls + factor(nchildren) + female |
    congress,
  data = analysis_data,
  weights = ~ I(1 / terms_observed), vcov = ~id
)
weighting <- bind_rows(
  tidy(member_congress, conf.int = TRUE) |>
    mutate(weighting = "member_congress"),
  tidy(equal_legislator, conf.int = TRUE) |>
    mutate(weighting = "equal_legislator")
) |>
  filter(term == "ngirls")
write_csv(weighting, file.path(output_dir, "weighting.csv"))

# Allow period-specific nuisance coefficients in slope comparisons.
period_model <- fixest::feols(
  aauw_all ~ ngirls * factor(washington_period + 2 * after_washington) +
    (factor(nchildren) + female) *
      factor(washington_period + 2 * after_washington) |
    congress,
  data = analysis_data, vcov = ~id
)
write_csv(
  tidy(period_model, conf.int = TRUE),
  file.path(output_dir, "period_contrasts.csv")
)

support <- analysis_data |> filter(!between(congress, 105, 108))
cohort_model <- fixest::feols(
  aauw_all ~ ngirls * washington_cohort +
    (factor(nchildren) + female) * washington_cohort |
    congress^washington_cohort,
  data = support, vcov = ~id
)
write_csv(
  tidy(cohort_model, conf.int = TRUE),
  file.path(output_dir, "cohort_contrasts.csv")
)

# Family size and sex composition are fixed throughout this released panel.
family_history <- frozen |> summarise(
  rows = n(), n_girl_values = n_distinct(ngirls),
  n_child_values = n_distinct(nchildren), .by = id
)
write_csv(family_history, file.path(output_dir, "family_history.csv"))
stopifnot(
  all(family_history$n_girl_values == 1),
  all(family_history$n_child_values == 1)
)
misclassification <- frozen |>
  summarise(rows = n(), legislators = n_distinct(id), .by = c(ngirls, anygirls))
write_csv(misclassification, file.path(output_dir, "daughter_indicator.csv"))

balance_data <- frozen |>
  arrange(id, congress) |>
  distinct(id, .keep_all = TRUE)
balance <- balance_data |>
  filter(nchildren != 12) |>
  group_by(nchildren) |>
  group_modify(\(data, key) {
    tidy(t.test(data$prop_girls, mu = .4878)) |>
      mutate(legislators = nrow(data), historical_n_label = parameter)
  }) |>
  ungroup()
write_csv(balance, file.path(output_dir, "balance_counts.csv"))
writeLines(
  capture.output(sessionInfo()),
  file.path(output_dir, "session_info.txt")
)
cat("Main table: all 20 coefficients and sample counts reproduce.\n")

# Rebuild early AAUW scores directly from the released vote codes.
bills <- read_csv("data/aauw_votes_97-116.csv", show_col_types = FALSE) |>
  filter(congress_or_senate == "congress", cong_number < 102) |>
  select(
    congress = cong_number, rollnumber = `Voteview Vote no.`, aauw_yes_or_no
  )
votes <- read_csv("data/HSall_votes.zip",
  show_col_types = FALSE,
  col_types = cols(
    .default = col_skip(), congress = col_double(),
    chamber = col_character(), icpsr = col_double(),
    rollnumber = col_double(), cast_code = col_double()
  )
) |>
  filter(between(congress, 97, 101))
member_ids <- read_csv(
  "data/voteview_congress_members.csv", show_col_types = FALSE
) |>
  filter(chamber == "House") |>
  select(congress, icpsr, id = bioguide_id) |>
  distinct()
upstream_results <- list()
upstream_pooled <- list()
vote_variants <- c(
  "historical_votes", "paired_as_abstention", "house_only", "both_vote_fixes"
)
for (variant in vote_variants) {
  source_votes <- if (variant %in% c("house_only", "both_vote_fixes")) {
    filter(votes, chamber == "House")
  } else {
    votes
  }
  scored <- left_join(bills, source_votes,
    by = c("congress", "rollnumber"),
    relationship = "many-to-many"
  ) |>
    mutate(vote = case_when(
      cast_code %in% 1:3 ~ 1, cast_code %in% 4:6 ~ -1,
      cast_code %in% 7:9 ~ 0, TRUE ~ NA_real_
    ))
  if (variant %in% c("paired_as_abstention", "both_vote_fixes")) {
    scored <- mutate(scored, vote = if_else(cast_code %in% c(2, 5), 0, vote))
  }
  new_scores <- scored |>
    mutate(support = case_when(
      aauw_yes_or_no == "yes" ~ as.numeric(vote == 1),
      aauw_yes_or_no == "no" ~ as.numeric(vote == -1),
      TRUE ~ NA_real_
    )) |>
    summarise(
      new_score = round(100 * mean(support, na.rm = TRUE)) / 100,
      .by = c(congress, icpsr)
    ) |>
    inner_join(member_ids,
      by = c("congress", "icpsr"), relationship = "many-to-many"
    ) |>
    select(congress, id, new_score)
  candidate <- left_join(frozen, new_scores,
    by = c("id", "congress"),
    relationship = "one-to-one"
  ) |>
    mutate(aauw_all = if_else(congress < 102, new_score, aauw_all))
  if (variant == "historical_votes") {
    stopifnot(isTRUE(all.equal(candidate$aauw_all, frozen$aauw_all)))
  }
  changed <- candidate$aauw_all != frozen$aauw_all
  effects <- candidate |>
    group_by(congress) |>
    group_modify(\(data, key) {
      tidy(lm(aauw_all ~ ngirls + factor(nchildren) + female, data = data)) |>
        filter(term == "ngirls")
    }) |>
    ungroup() |>
    mutate(variant)
  upstream_results[[variant]] <- effects
  analysis_data <- candidate |>
    drop_na(aauw_all, ngirls, nchildren, female, congress, id)
  model <- fixest::feols(
    aauw_all ~ ngirls + factor(nchildren) + female | congress,
    data = analysis_data, vcov = ~id
  )
  upstream_pooled[[variant]] <- tidy(model, conf.int = TRUE) |>
    filter(term == "ngirls") |>
    mutate(
      variant,
      changed_scores = sum(changed, na.rm = TRUE), rows = nobs(model)
    )
  cat(
    variant, ": changed scores =", sum(changed, na.rm = TRUE),
    "; pooled slope =", coef(model)["ngirls"], "\n"
  )
  write_csv(
    candidate |>
      select(id, congress, aauw_all) |>
      mutate(previous_score = frozen$aauw_all) |>
      filter(aauw_all != previous_score),
    file.path(output_dir, paste0(variant, "_changed_scores.csv"))
  )
}
write_csv(
  bind_rows(upstream_pooled),
  file.path(output_dir, "early_vote_pooled.csv")
)
write_csv(
  bind_rows(upstream_results),
  file.path(output_dir, "early_vote_comparisons.csv")
)

# Cohort membership means actual House service, not any congressional service.
house_ids <- member_ids |>
  filter(between(congress, 105, 108)) |>
  pull(id) |>
  unique()
cohort_members <- frozen |>
  distinct(id, first_name, last_name) |>
  mutate(
    historical_cohort = id %in% cohort_ids, house_cohort = id %in% house_ids
  )
write_csv(
  filter(cohort_members, historical_cohort != house_cohort),
  file.path(output_dir, "cohort_membership_changes.csv")
)
cohort_models <- list()
cohort_tests <- list()
cohort_gaps <- list()
for (definition in c("historical", "house_service")) {
  selected_ids <- if (definition == "historical") cohort_ids else house_ids
  analysis_data <- frozen |>
    drop_na(aauw_all, ngirls, nchildren, female, congress, id) |>
    mutate(
      washington_cohort = as.integer(id %in% selected_ids),
      time = congress - 105
    )
  cohort_models[[definition]] <- analysis_data |>
    group_by(congress, washington_cohort) |>
    group_modify(\(data, key) {
      tidy(lm(aauw_all ~ ngirls + factor(nchildren) + female, data = data)) |>
        filter(term == "ngirls") |>
        mutate(rows = nrow(data))
    }) |>
    ungroup() |>
    mutate(definition)
  comparison <- filter(analysis_data, !between(congress, 105, 108))
  gap <- fixest::feols(
    aauw_all ~ ngirls * washington_cohort +
      (factor(nchildren) + female) * washington_cohort |
      congress^washington_cohort,
    data = comparison, vcov = ~id
  )
  cohort_gaps[[definition]] <- tidy(gap, conf.int = TRUE) |>
    filter(term == "ngirls:washington_cohort") |>
    mutate(definition)
  for (cohort in c(0, 1)) {
    cohort_sample <- filter(analysis_data, washington_cohort == cohort)
    varying <- fixest::feols(
      aauw_all ~ (ngirls + factor(nchildren) + female) * factor(congress),
      data = cohort_sample, vcov = ~id
    )
    linear <- fixest::feols(
      aauw_all ~ ngirls * time + factor(nchildren) + female | congress,
      data = cohort_sample, vcov = ~id
    )
    cat("Cohort", cohort, definition, "unrestricted slope constancy:\n")
    joint <- fixest::wald(varying, keep = "ngirls:factor", print = FALSE)
    trend <- tidy(linear, conf.int = TRUE) |> filter(term == "ngirls:time")
    cohort_tests[[paste(definition, cohort)]] <- tibble(
      definition, cohort,
      f_statistic = joint$stat, p_joint = joint$p,
      df1 = joint$df1, df2 = joint$df2,
      annual_trend = trend$estimate, p_trend = trend$p.value
    )
    print(cohort_tests[[paste(definition, cohort)]])
  }
}
write_csv(
  bind_rows(cohort_models),
  file.path(output_dir, "cohort_definition_comparisons.csv")
)

# Compare the printed per-daughter label with the fitted exposure.
si_cohort_examples <- tibble(
  congress = c(105, 108, 116), printed = c(.156, .094, .145)
) |>
  left_join(filter(cohort_results, washington_cohort),
    by = "congress",
    relationship = "one-to-many"
  ) |>
  select(congress, printed, exposure, estimate)
proportion_examples <- filter(si_cohort_examples, exposure == "prop_girls")
stopifnot(
  all(round(proportion_examples$estimate, 3) == proportion_examples$printed)
)
write_csv(si_cohort_examples, file.path(output_dir, "si_cohort_labels.csv"))
cat("Upstream corrections and cohort definitions independently checked.\n")

write_csv(bind_rows(cohort_tests), file.path(output_dir, "cohort_tests.csv"))
write_csv(bind_rows(cohort_gaps), file.path(output_dir, "cohort_gaps.csv"))

analysis_data <- frozen |>
  drop_na(aauw_all, ngirls, nchildren, female, congress, id)
model <- lm(aauw_all ~ ngirls + factor(nchildren) + female + factor(congress),
  data = analysis_data
)
reversed_model <- update(model, data = slice(analysis_data, rev(seq_len(n()))))
stopifnot(isTRUE(all.equal(coef(model), coef(reversed_model))))
