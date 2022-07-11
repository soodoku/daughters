## Congressional List
## Get list of MCs from bioguide
## Merge with Voteview

### Set working dir
setwd(githubdir)
setwd("daughters/")

### Load libs
library(tidyverse)
library(babynames)

# official cong list from https://bioguide.congress.gov/
# concatenate to produce the output file
official_cong_list <- read_csv("data/member_id/out/bio_97_to_117.csv") %>% 
  select(-`...1`, -congresses, -biography, -position) 

official_cong_list[duplicated(official_cong_list[, c("congress", "id")]), ] # check all ids are uniques

official_cong_list <- official_cong_list %>%
  mutate(last_name = gsub(", Jr.", "", unaccentedFamilyName)) 

# voteview data on congressional district, dw nominate scores
# downloaded from voteview.com
voteview_cong_list <- read_csv("data/voteview_congress_members.csv") %>%
  #%>% select(-X23) 
  filter(!chamber %in% c("President", "Senate"))

dupes <- voteview_cong_list[duplicated(voteview_cong_list[, c("bioguide_id", "congress")]), ]$bioguide_id
voteview_dupes <- voteview_cong_list %>% filter(bioguide_id %in% dupes, chamber == "House") %>% arrange(bioguide_id)

voteview_cong_list <- voteview_cong_list %>%
  filter(!duplicated(voteview_cong_list[, c("bioguide_id", "congress", "chamber")])) 

official_cong_list <- official_cong_list %>%
  select(congress, id, last_name, first_name = unaccentedGivenName, 
         middle_name = unaccentedMiddleName, nick_name = nickName) %>%
  left_join(voteview_cong_list, by = c("id" = "bioguide_id", "congress")) %>%
  dplyr::rename(state = state_abbrev,
                district = district_code) %>%
  mutate(party = ifelse(party_code == 100, "D",
                        ifelse(party_code == 200, "R",
                               ifelse(party_code == 328, "I", NA)))) %>%
  select(congress, chamber, state, district, party, id, last_name, first_name, middle_name, nick_name, icpsr,
         born, died, nominate_dim1, nominate_dim2)

official_cong_list <- official_cong_list %>%
  select(congress, chamber, state, district, party, id, last_name, first_name, middle_name, nick_name, icpsr, 
         born, died, nominate_dim1, nominate_dim2)
  
# check ----
official_cong_list %>%
  filter(chamber != "President") %>%
  group_by(congress, chamber) %>%
  tally() %>%
  ggplot(aes(congress, n, fill = chamber)) +
  geom_bar(stat = "identity", position = "dodge")
  
write_csv(official_cong_list, "data/official_cong_list.csv")
