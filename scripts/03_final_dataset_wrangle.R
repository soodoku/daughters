## Final Dataset

### Load libs
library(tidyverse)

# ------------ load ------------
bio <- read_csv("data/official_cong_list.csv")
child_master <- read_csv("data/Child Info Master List Dotters.csv")
female <- read_csv("data/female members of congress.csv")

# pre-wrangled aauw
aauw_post101_all_issues <- read_csv("data/aauw_scores_long.csv") |>
  rename(aauw_all = aauw_support_all, aauw_voting = aauw_support_voting)
aauw_post101_women <- read_csv("data/aauw_womens.csv") |>
  rename(
    aauw_women_voting = aauw_support_voting,
    aauw_women_all = aauw_support_all
  )

aauw_post101 <- left_join(
  aauw_post101_all_issues,
  aauw_post101_women,
  by = c("congress", "id")
)

# aauw bill info
aauw_bills <- read_csv("data/aauw_votes_97-116.csv") |>
  filter(congress_or_senate == "congress") # keep only House records


# voteview info
votes <- read_csv("data/HSall_votes.zip") |>
  filter(congress > 96, chamber == "House") # House roll calls

member_info <- read_csv(
  "data/voteview_congress_members.csv",
  col_types = cols(Check = col_character())
)


# wrangle pre-102 aauw

pre_102_votes <- aauw_bills |>
  # organize bills list
  filter(cong_number < 102) |>
  group_by(cong_number) |>
  mutate(aauw_id = row_number()) |>
  ungroup() |>
  select(
    aauw_id,
    congress = cong_number,
    aauw_yes_or_no,
    voteview_rollnumber = `Voteview Vote no.`,
    womens_issue = `legislation related to women's issues (0 or 1)`
  ) |>
  # join with votes
  left_join(votes, by = c("congress", "voteview_rollnumber" = "rollnumber")) |>
  # recode votes
  mutate(
    vote = ifelse(
      cast_code %in% c(1, 3),
      1,
      ifelse(
        cast_code %in% c(4, 6),
        -1,
        ifelse(cast_code %in% c(2, 5, 7:9), 0, NA)
      )
    ),
    aauw_score = ifelse(
      aauw_yes_or_no == "yes",
      vote,
      ifelse(aauw_yes_or_no == "no", vote * -1, NA)
    ),
    aauw_score = ifelse(
      aauw_score == 1,
      "R",
      ifelse(aauw_score == -1, "W", ifelse(aauw_score == 0, "A", NA))
    )
  ) |>
  arrange(congress, aauw_id) |>
  select(congress, icpsr, aauw_id, aauw_score, womens_issue)

aauw_pre102_all_issues <- pre_102_votes |>
  group_by(congress, icpsr, aauw_score) |>
  tally() |>
  ungroup() |>
  pivot_wider(names_from = aauw_score, values_from = n) |>
  mutate(
    R = ifelse(is.na(R), 0, R),
    W = ifelse(is.na(W), 0, W),
    A = ifelse(is.na(A), 0, A),
    total_voting = R + W,
    total_all = R + W + A,
    aauw_voting = round((R / total_voting) * 100, digits = 0),
    aauw_all = round((R / total_all) * 100, digits = 0)
  ) |>
  select(congress, icpsr, aauw_voting, aauw_all)

# aauw for only women's issues

aauw_pre102_women <- pre_102_votes |>
  filter(womens_issue == 1) |>
  group_by(congress, icpsr, aauw_score) |>
  tally() |>
  ungroup() |>
  pivot_wider(names_from = aauw_score, values_from = n) |>
  mutate(
    R = ifelse(is.na(R), 0, R),
    W = ifelse(is.na(W), 0, W),
    A = ifelse(is.na(A), 0, A),
    total_voting = R + W,
    total_all = R + W + A,
    aauw_women_voting = round((R / total_voting) * 100, digits = 0),
    aauw_women_all = round((R / total_all) * 100, digits = 0)
  ) |>
  select(congress, icpsr, aauw_women_voting, aauw_women_all)

aauw_pre102 <- left_join(
  aauw_pre102_all_issues,
  aauw_pre102_women,
  by = c("congress", "icpsr")
) |>
  left_join(
    member_info |>
      filter(chamber == "House") |>
      select(icpsr, id = bioguide_id, congress),
    by = c("congress", "icpsr")
  ) |>
  select(-icpsr)

aauw_final <- rbind(aauw_pre102, aauw_post101)

table(is.na(aauw_final$id), aauw_final$congress)

aauw_final <- filter(aauw_final, !is.na(id))

# keep only uniques from children list
children <- child_master |>
  mutate(high_confidence = ifelse(is.na(oliver1_ngirls), 0, 1)) |>
  select(id, ngirls, nboys, nchildren, high_confidence) |>
  filter(!duplicated(id))

# final data
d <- bio |>
  mutate(female = ifelse(id %in% female$`member ID`, 1, 0)) |>
  left_join(
    children |> select(id, ngirls, nboys, nchildren, high_confidence),
    by = "id"
  ) |>
  left_join(aauw_final, by = c("id", "congress")) |>
  mutate(anygirls = as.integer(ngirls > 0)) |>
  filter(nchildren > 0)

stopifnot(!anyDuplicated(d[c("id", "congress")]))
write_csv(d, "data/final_data_2022_01_05.csv")
