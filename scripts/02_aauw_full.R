## Data Analysis

### Set working dir

### Load libs
library(tidyverse)
library(ggplot2)
library(stringi)

# load data ----

official_cong_list <- read_csv("data/official_cong_list.csv")

rc101_108 <- read_csv("data/aauw/aauw_votes/101_to_108_rollcall.csv")
rc109_114 <- read_csv("data/aauw/aauw_votes/109_to_114_rollcall.csv")
rc115_116 <- read_csv("data/aauw/aauw_votes/115_and_116_rollcall.csv")
colnames(rc101_108) <- tolower(colnames(rc101_108))
colnames(rc115_116) <- tolower(colnames(rc115_116))
cos109_114 <- read_csv("data/aauw/aauw_votes/109_to_114_cosponsorship.csv")
cos115_116 <- read_csv("data/aauw/aauw_votes/115_and_116_cosponsorship.csv")

aauw <- read_csv("data/aauw/aauw_votes_97-116.csv")

states <- as_tibble(cbind(state.name, state.abb))

# join data ----
rollcall <- rbind(rc101_108, rc109_114, rc115_116) |>
  mutate(
    url_parse = gsub("[^A-z0-9+]", " ", bio_url),
    id = word(url_parse, -1)
  ) |>
  select(-bio_url, -url_parse) |>
  filter(!is.na(vote)) |>
  arrange(congress, state, representative, roll_no)

rollcall$representative <- iconv(
  rollcall$representative,
  "ASCII",
  "UTF-8",
  sub = "byte"
)

rollcall <- rollcall |>
  mutate(
    parse_name = gsub("\\s*\\([^\\)]+\\)", "", representative),
    last_name = gsub("(.*),.*", "\\1", parse_name),
    first_name = str_remove(parse_name, last_name),
    first_name = gsub(", ", "", first_name),
    state_parsed = str_remove(representative, last_name),
    state_parsed = gsub("\\(|\\)", "", state_parsed),
    state_parsed = gsub(" ", "", state_parsed),
    state = ifelse(state == "(Unknown)", state_parsed, state)
  ) |>
  left_join(states, by = c("state" = "state.name")) |>
  mutate(
    state_new = ifelse(state %in% state.abb, state, state.abb),
    state = state_new
  ) |>
  select(-state_new, -state.abb, -state_parsed) |>
  filter(!is.na(state))

cosponsor <- rbind(cos109_114, cos115_116) |>
  mutate(
    url_parse = gsub("[^A-z0-9+]", " ", bio_url),
    id = word(url_parse, -1)
  ) |>
  select(-bio_url, -url_parse) |>
  mutate(bill = gsub(" ", "", bill))


# find ids where NA ----

rc_no_id <- rollcall |>
  filter(is.na(id)) |>
  select(congress, representative, last_name, first_name, state, party) |>
  group_by(congress, state, representative) |>
  unique() |>
  ungroup()

# where no ids parsed, join offical_cong_list via congress, state, last_name
rc_no_id_unique <- rc_no_id |>
  filter(first_name == "") |>
  mutate(
    last_name = toupper(last_name),
    last_name = ifelse(
      last_name == "LAMBERT" & state == "AR",
      "LINCOLN",
      last_name
    ),
    last_name = ifelse(
      last_name == "CHENOWETH" & state == "ID",
      toupper("Chenoweth-Hage"),
      last_name
    ),
    last_name = ifelse(
      last_name == "JACKSON-LEE" & state == "TX",
      "JACKSON LEE",
      last_name
    ),
    last_name = ifelse(
      last_name == "WALDHOLTZ" & state == "UT",
      toupper("Greene Waldholtz"),
      last_name
    ),
    last_name = ifelse(
      last_name == "HERSETH" & state == "SD",
      toupper("Herseth Sandlin"),
      last_name
    )
  ) |>
  left_join(
    official_cong_list |>
      select(congress, state, last_name, id) |>
      mutate(last_name = toupper(last_name)),
    by = c("congress", "state", "last_name")
  )

# where no ids for multiple periods, correct name variable for joining
rc_no_id_dupe <- rc_no_id |>
  filter(first_name != "") |>
  mutate(
    last_name = toupper(last_name),
    first_name = toupper(first_name),
    first_name = ifelse(
      last_name == "JOHNSON" & first_name == "E. B." & state == "TX",
      toupper("Eddie"),
      first_name
    ),
    first_name = ifelse(
      last_name == "SCHAEFER" & first_name == "DAN" & state == "CO",
      toupper("Daniel"),
      first_name
    ),
    first_name = ifelse(
      last_name == "SCHAFFER" & first_name == "BOB" & state == "CO",
      toupper("Robert"),
      first_name
    ),
    first_name = ifelse(
      last_name == "DAVIS" & first_name == "JO ANN" & state == "VA",
      toupper("Jo"),
      first_name
    ),
    first_name = ifelse(
      last_name == "DAVIS" & first_name == "TOM" & state == "VA",
      toupper("Thomas"),
      first_name
    ),
    first_name = ifelse(
      last_name == "MILLER" & first_name == "DAN" & state == "FL",
      toupper("Daniel"),
      first_name
    ),
    first_name = ifelse(
      last_name == "DIAZ-BALART" & first_name == "L." & state == "FL",
      toupper("Lincoln"),
      first_name
    ),
    first_name = ifelse(
      last_name == "DIAZ-BALART" & first_name == "M." & state == "FL",
      toupper("Mario"),
      first_name
    ),
    first_name = ifelse(
      last_name == "SANCHEZ" & first_name == "LINDA T." & state == "CA",
      toupper("Linda"),
      first_name
    )
  ) |>
  left_join(
    official_cong_list |>
      select(congress, state, last_name, first_name, id) |>
      mutate(
        last_name = toupper(last_name),
        first_name = toupper(first_name)
      ),
    by = c("congress", "state", "last_name", "first_name")
  ) |>
  rename(id1 = id) |>
  left_join(
    official_cong_list |>
      select(congress, state, last_name, nick_name, id) |>
      mutate(
        last_name = toupper(last_name),
        nick_name = toupper(nick_name)
      ),
    by = c("congress", "state", "last_name", "first_name" = "nick_name")
  ) |>
  rename(id2 = id) |>
  left_join(
    official_cong_list |>
      select(congress, state, last_name, middle_name, id) |>
      mutate(
        last_name = toupper(last_name),
        middle_name = toupper(middle_name)
      ),
    by = c("congress", "state", "last_name", "first_name" = "middle_name")
  ) |>
  rename(id3 = id) |>
  mutate(id = coalesce(id1, id2, id3))

rc_no_id_joined <- rbind(
  rc_no_id_unique |> select(congress, representative, state, id),
  rc_no_id_dupe |> select(congress, representative, state, id)
)


rollcall_final <- rollcall |>
  left_join(
    rc_no_id_joined |> rename(id_new = id),
    by = c("congress", "state", "representative")
  ) |>
  mutate(id = ifelse(is.na(id), id_new, id)) |>
  select(-parse_name, -last_name, -first_name, -id_new)


# wrangle aauw bill list ----

# wrangle working aauw document
# grab roll call/ session info from doc, remove urls/ comments
aauw_new <- aauw |>
  mutate(
    roll_call_no = gsub("[^0-9+]", "", `Vote no.`),
    roll_call_no = as.numeric(roll_call_no),
    roll_call_no = ifelse(roll_call_no > 1000, NA, roll_call_no),
    session = ifelse(
      grepl("First Session", `Vote no.`),
      1,
      ifelse(grepl("Second Session", `Vote no.`), 2, NA)
    ),
    type = ifelse(grepl("sponsorship", `Vote no.`), "cosponsorship", "vote"),
    chamber = ifelse(
      congress_or_senate == "congress",
      "House",
      ifelse(congress_or_senate == "senate", "Senate", NA)
    ),
    aauw_pro_against = aauw_yes_or_no,
    aauw_pro_against = ifelse(
      aauw_pro_against == "yes",
      1,
      ifelse(aauw_pro_against == "no", -1, NA)
    ),
    bill_id = gsub(" ", "", bill_id)
  ) |>
  rename(
    congress = cong_number,
    womens_issue = `legislation related to women's issues (0 or 1)`
  ) |>
  group_by(congress, chamber) |>
  mutate(aauw_id = row_number()) |>
  ungroup() |>
  select(
    `Vote no.`,
    aauw_id,
    bill_id,
    congress,
    chamber,
    type,
    session,
    roll_call_no,
    aauw_pro_against,
    womens_issue
  ) |>
  mutate(
    aauw_pro_against = ifelse(
      congress == 113 & aauw_id == 8,
      1,
      aauw_pro_against
    )
  )


# wrangle roll call ----
aauw_votes <- rollcall_final |>
  mutate(
    vote = ifelse(
      vote %in% c("YEA", "AYE"),
      1,
      ifelse(
        vote %in% c("NAY", "NO"),
        -1,
        ifelse(vote %in% c("PRESENT", "NOT VOTING"), 2, NA)
      )
    ),
    # aauw does not include session until 107, remove for joining
    session = ifelse(congress < 107, NA, session)
  ) |>
  left_join(
    aauw_new |>
      filter(
        type == "vote",
        chamber == "House"
      ) |>
      select(-chamber),
    by = c("roll_no" = "roll_call_no", "session", "congress")
  ) |>
  # change vote to right/ wrong based on aauw position
  mutate(
    aauw_score = ifelse(vote %in% c(1, -1), vote * aauw_pro_against, vote),
    aauw_score = ifelse(
      aauw_score == 1,
      "R",
      ifelse(aauw_score == -1, "W", ifelse(aauw_score == 2, "A", NA))
    )
  ) |>
  arrange(congress, aauw_id) |>
  select(congress, id, aauw_id, type, aauw_score)


# wrangle cosponsorships ----

aauw_cosponsor <-
  # start with list of full members
  official_cong_list |>
  # cosponsorships begin in 111th congress
  filter(chamber == "House", congress > 110 & congress < 117) |>
  select(congress, state, district, last_name, first_name, id, chamber) |>
  # create obs for each cosponsorship opp
  left_join(
    aauw_new |>
      filter(
        type == "cosponsorship",
        chamber == "House"
      ) |>
      select(-chamber),
    by = c("congress")
  ) |>
  # Match cosponsors by Congress, member and bill.
  left_join(
    cosponsor |>
      mutate(vote = 1) |>
      select(bill_id = bill, id, congress, vote),
    by = c("congress", "id", "bill_id")
  ) |>
  # change cosponsorship to right/ wrong per aauw stance
  mutate(
    vote = ifelse(is.na(vote), -1, 1),
    aauw_score = vote * aauw_pro_against,
    vote,
    aauw_score = ifelse(aauw_score == 1, "R", ifelse(aauw_score == -1, "W", NA))
  ) |>
  select(congress, id, aauw_id, type, aauw_score)


# join votes + cosponsorship ----

joined_df <- rbind(aauw_votes, aauw_cosponsor) |>
  select(-type) |>
  left_join(
    official_cong_list |>
      filter(chamber == "House") |>
      select(state, district, id, last_name, first_name, congress),
    by = c("id", "congress")
  ) |>
  ##### where are there na last names???????
  filter(!is.na(last_name)) |>
  arrange(congress, id, aauw_id) |>
  group_by(congress, id, aauw_id) |>
  unique() |>
  ungroup() |>
  filter(
    !(congress == 101 & id == "S000527" & is.na(aauw_score)),
    congress != 101
  )

# in aauw scorecard format -- compare against aauw scorecards to confirm
aauw_scorecard <- joined_df |>
  group_by(congress, id) |>
  pivot_wider(
    names_from = aauw_id,
    names_prefix = "issue_",
    values_from = aauw_score
  ) |>
  ungroup() |>
  arrange(congress, match(state, state.abb), district) |>
  select(congress, state, district, last_name, first_name, id, issue_1:issue_11)

table(aauw_scorecard$congress)


# calculate aauw scores
aauw_scores_long <- joined_df |>
  group_by(congress, id, aauw_score) |>
  tally() |>
  ungroup() |>
  filter(!is.na(aauw_score)) |>
  pivot_wider(names_from = aauw_score, values_from = n) |>
  mutate(
    R = ifelse(is.na(R), 0, R),
    W = ifelse(is.na(W), 0, W),
    A = ifelse(is.na(A), 0, A),
    total_voting = R + W,
    total_all = R + W + A,
    aauw_support_voting = round((R / total_voting) * 100, digits = 0),
    aauw_support_all = round((R / total_all) * 100, digits = 0)
  ) |>
  select(congress, id, aauw_support_voting, aauw_support_all)

# aauw for only women's issues

aauw_womens <- joined_df |>
  left_join(
    aauw_new |>
      filter(chamber == "House") |>
      select(congress, aauw_id, womens_issue),
    by = c("congress", "aauw_id")
  ) |>
  filter(womens_issue == 1) |>
  group_by(congress, id, aauw_score) |>
  tally() |>
  ungroup() |>
  filter(!is.na(aauw_score)) |>
  pivot_wider(names_from = aauw_score, values_from = n) |>
  mutate(
    R = ifelse(is.na(R), 0, R),
    W = ifelse(is.na(W), 0, W),
    A = ifelse(is.na(A), 0, A),
    total_voting = R + W,
    total_all = R + W + A,
    aauw_support_voting = round((R / total_voting) * 100, digits = 0),
    aauw_support_all = round((R / total_all) * 100, digits = 0)
  ) |>
  select(congress, id, aauw_support_voting, aauw_support_all)


write_csv(aauw_scores_long, "data/aauw_scores_long.csv")
write_csv(aauw_womens, "data/aauw_womens.csv")
