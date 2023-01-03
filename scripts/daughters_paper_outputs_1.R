## Data Analysis
## Table 1: Effect of ndaughters over time
## Figure 1: Effect of ndaughters over time among Washington Cohort/Rest
## Does the effect among the rest of the data (minus Washington) overlap the CI of the "original"

### Set working dir
setwd("~/Documents/Github/daughters/")

### Load libs
library(stargazer)
library(tidyverse)
library(fwildclusterboot)
library(knitr)
library(lme4)

# Set seed
set.seed(1234567)

# Load data
nominate_scores <- read_csv("data/voteview_congress_members.csv") %>%
  filter(chamber == "House") %>%
  select(id = bioguide_id, congress, nokken_poole_dim1) 

d <- read_csv("data/final_data_2022_01_05.csv") %>%
  left_join(nominate_scores, by = c("id", "congress")) %>%
  mutate(aauw_all = aauw_all/100,
         aauw_women_all = aauw_women_all/100,
         nominate_dim1 = -nominate_dim1,
         nokken_poole_dim1 = - nokken_poole_dim1,
         prop_girls = ngirls/nchildren)

### AAUW Voting (Changed the y to aauw_women_voting but same analysis as above)

d %>%
  filter(party != "I") %>%  # too few I to use geom_smooth
  mutate(party = ifelse(party == "D", "Democratic", "Republican")) %>%
  ggplot(aes(congress, aauw_women_voting, color = party)) +
  geom_point(position = "jitter", alpha = 0.3, size = 0.7) + 
  geom_smooth() +
  scale_color_manual(values = c("Democratic" = "blue", "Republican" = "red")) +
  theme_minimal() +
  labs(title = "AAUW by Congress", x = "Congress", y = "AAUW", color = "") 

## All MCs/Table 1
## AAUW All (including cosponsorships), Ngirls

congress <- as.character(c(97:116))

fitted_ngirls_all <- d %>% 
  group_by(congress) %>% 
  do(aauw_model = lm(aauw_all ~ ngirls + as.factor(nchildren) + female, data = .)) %>%
  mutate(co_aauw = coef(aauw_model)["ngirls"],
         se_aauw = coef(summary(aauw_model))["ngirls", "Std. Error"])

stargazer(fitted_ngirls_all$aauw_model,
          column.labels = congress, 
          covariate.labels = "N. Daughters",
          dep.var.labels = "AAUW",
          omit = c("nchildren", "female", "Constant"),
          header = FALSE,
          type = "latex",
          omit.stat=c("LL","ser","f", "rsq"), 
          float.env = "sidewaystable",
          font.size = "tiny",
          out = "tabs/table_1_ngirls_aauw_by_cong.tex",
          caption = "Estimated Average Treatment Effect Among All MCs")

### Add party
fitted_ngirls_all_party <- d %>% 
  group_by(congress) %>% 
  do(aauw_model = lm(aauw_all ~ ngirls + as.factor(nchildren) + party + female, data = .)) %>%
  mutate(co_aauw = coef(aauw_model)["ngirls"],
         se_aauw = coef(summary(aauw_model))["ngirls", "Std. Error"])

stargazer(fitted_ngirls_all_party$aauw_model,
          column.labels = congress, 
          covariate.labels = "N. Daughters",
          dep.var.labels = "AAUW",
          omit = c("nchildren", "female", "Constant"),
          header = FALSE,
          type = "latex",
          omit.stat=c("LL","ser","f", "rsq"), 
          float.env = "sidewaystable",
          font.size = "tiny",
          out = "tabs/tab_1_pid.tex",
          caption = "Effect of the Number of Daughters on AAUW Score Controlling  for  Indicator  Variables  for the Number  of Children, MC's gender and party")

### Pooled Reg
aauw_ngirls <- lm(aauw_all ~ ngirls + as.factor(congress) + as.factor(nchildren) + female, d)
boot_aauw_ngirls <- boottest(aauw_ngirls, clustid = "id", param = "ngirls", B = 9999)
boot_aauw <- data.frame(rbind(boot_aauw_ngirls)) %>% 
  select(point_estimate, conf_int, N) %>%
  unnest() %>%
  cbind(conf_int_lab = rep(c("ci min", "ci max"), 1)) %>%
  pivot_wider(names_from = conf_int_lab, values_from = conf_int)

kable(boot_aauw, digits = 3)

### Hierarchical Model/Pooled
aauw_ngirls_hier <- lmer(aauw_all ~ ngirls + as.factor(congress) + as.factor(nchildren) + female + (1|id), d)

stargazer(aauw_ngirls_hier,
          covariate.labels = "N. Daughters",
          dep.var.labels = "AAUW",
          omit = c("nchildren", "female", "congress", "Constant"),
          header = FALSE,
          type = "latex",
          omit.stat=c("LL","ser","f", "rsq"), 
          out = "tabs/table_1a_ngirls_aauw_pooled_hier.tex",
          caption = "Estimated Average Treatment Effect Among All MCs Using a Random Effects Hierarchical Model")

## EB's cohort vs. rest./Figure 1

ebonya_cohort <- unique(d[d$congress %in% c(105:108),]$id)

d <- d %>%
  mutate(ebonya_cohort = ifelse(id %in% ebonya_cohort, "Washington", "Non-Washington"))

fitted_ngirls <- d %>% 
  group_by(congress, ebonya_cohort) %>% 
  do(aauw_model = lm(aauw_all ~ ngirls + as.factor(nchildren) + female, data = .)) %>%
  mutate(co_aauw = coef(aauw_model)["ngirls"],
         se_aauw = coef(summary(aauw_model))["ngirls", "Std. Error"])

fitted_ngirls %>%
  pivot_longer(cols = c(co_aauw:se_aauw), names_to = c("type", "dep_var"), names_sep = "_") %>%
  pivot_wider(names_from = type) %>%
  ggplot(aes(congress, co)) +
  geom_rect(aes(xmin=105, xmax=108, ymin=-Inf, ymax=Inf), fill="lightgrey", alpha=0.5) +
  geom_rect(aes(xmin=110, xmax=114, ymin=-Inf, ymax=Inf), fill="lightgrey", alpha=0.5) +
  geom_text(aes(x = 106.5, y = 0.3, label = "Washington")) +
  geom_text(aes(x = 112, y = 0.3, label = "Costa et al.")) +
  geom_line(aes(color = (ebonya_cohort))) +
  geom_pointrange(aes(ymin=co-1.96*se, ymax=co + 1.96*se, color = as.factor(ebonya_cohort)), alpha = 0.7, 
                 position=position_dodge(0.05)) + 
  #geom_point(aes(size = se, color = as.factor(ebonya_cohort)), alpha = 0.7) +
  labs(title = "Estimated Average Treatment Effect of Number of Daughters",
       x = "Congress", y = "AAUW", size = "Standard Error", color = "Cohort") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.box = "vertical")

ggsave("figs/fig_1_ebonya_cohort.pdf")

## __Ebonya__ AAUW Scores on Proportion of Daughters

stargazer(fitted_ngirls[fitted_ngirls$ebonya_cohort == "Washington", ]$aauw_model,
          column.labels = congress,covariate.labels = "N. Daughters",
          dep.var.labels = "AAUW",
          omit = c("nchildren", "female", "Constant"),
          header = FALSE,
          type = "latex",
          omit.stat=c("LL","ser","f", "rsq"), 
          float.env = "sidewaystable",
          label = "Estimated Average Treatment Effect Among Washington Cohort")

## __Non-Ebonya__ AAUW Scores on Proportion of Daughters

stargazer(fitted_ngirls[fitted_ngirls$ebonya_cohort == "Non-Washington", ]$aauw_model,
          column.labels = congress,covariate.labels = "N. Daughters",
          dep.var.labels = "AAUW",
          omit = c("nchildren", "female", "Constant"),
          header = FALSE,
          type = "latex",
          omit.stat=c("LL","ser","f", "rsq"), 
          caption = "Estimated Average Treatment Effect Among Non-Washington Cohort")

## Does the effect among the rest of the data (minus Washington) overlap the CI of the "original" --- YES

ebonya_cong   <- d$congress %in% c(105:108)

d <- d %>%
  mutate(ebonya_cong = ifelse(congress %in% c(105:108), "Washington Congress", "Non-Washington Congress"))

nw_cong  <- lm(aauw_all ~ ngirls + as.factor(nchildren) + female + as.factor(congress), data = d[d$ebonya_cong == "Non-Washington Congress", ])
w_cong   <- lm(aauw_all ~ ngirls + as.factor(nchildren) + female + as.factor(congress), data = d[d$ebonya_cong == "Washington Congress", ])

boot_nw_cong  <- boottest(nw_cong, clustid = "id", param = "ngirls", B = 9999)
boot_w_cong   <- boottest(w_cong, clustid = "id", param = "ngirls", B = 9999)

boot_aauw <- data.frame(rbind(boot_nw_cong, boot_w_cong)) %>% 
  mutate(model = rownames(rbind(boot_nw_cong, boot_w_cong))) %>%
  select(model, point_estimate, conf_int, t_stat) %>%
  unnest() %>%
  cbind(conf_int_lab = rep(c("ci min", "ci max"), 2)) %>%
  pivot_wider(names_from = conf_int_lab, values_from = conf_int)

## See the effect of including non-biological children

non_bio <- read.csv("data/children_with_non_biological_washington_cohort.csv")
non_bio <- non_bio[!duplicated(non_bio), ]
non_bio <- non_bio[non_bio$id != "", ]
# sanity check before nuking dupes: non_bio %>% group_by_at(vars(icpsr)) %>% filter(n()>1)
non_bio <- non_bio[!duplicated(non_bio$icpsr), ]

# Merge with washington
ew_cong <- d[d$congress %in% c(105:108), ]

ew_cong_m <- ew_cong %>% 
  left_join(non_bio[, c("icpsr", "ngirls_total", "nchildren_total")])

w_cong_bio   <- lm(aauw_all ~ ngirls + as.factor(nchildren) + female + as.factor(congress), data = ew_cong_m)
boot_w_cong_bio   <- boottest(w_cong_bio, clustid = "id", param = "ngirls", B = 9999)

w_cong_non_bio   <- lm(aauw_all ~ ngirls_total + as.factor(nchildren_total) + female + as.factor(congress), data = ew_cong_m)
boot_w_cong_non_bio   <- boottest(w_cong_non_bio, clustid = "id", param = "ngirls_total", B = 9999)

fitted_ngirls_all_bio <- d[d$congress %in% c(105:108), ] %>% 
  group_by(congress) %>% 
  do(aauw_model = lm(aauw_all ~ ngirls + as.factor(nchildren) + female, data = .)) %>%
  mutate(co_aauw = coef(aauw_model)["ngirls"],
         se_aauw = coef(summary(aauw_model))["ngirls", "Std. Error"])

fitted_ngirls_all_nonbio <- ew_cong_m %>% 
  group_by(congress) %>% 
  do(aauw_model = lm(aauw_all ~ ngirls_total + as.factor(nchildren_total) + female, data = .)) %>%
  mutate(co_aauw = coef(aauw_model)["ngirls_total"],
         se_aauw = coef(summary(aauw_model))["ngirls_total", "Std. Error"])

# Stargazer is being finicky so let's get around it
non_bio_105 <- lm(aauw_all ~ ngirls_total + as.factor(nchildren_total) + female, data = ew_cong_m[ew_cong_m$congress %in% 105, ])
non_bio_106 <- lm(aauw_all ~ ngirls_total + as.factor(nchildren_total) + female, data = ew_cong_m[ew_cong_m$congress %in% 106, ])
non_bio_107 <- lm(aauw_all ~ ngirls_total + as.factor(nchildren_total) + female, data = ew_cong_m[ew_cong_m$congress %in% 107, ])
non_bio_108 <- lm(aauw_all ~ ngirls_total + as.factor(nchildren_total) + female, data = ew_cong_m[ew_cong_m$congress %in% 108, ])

stargazer(non_bio_105, non_bio_106, non_bio_107, non_bio_108, w_cong_non_bio,
          column.labels = c(105:108, "Pooled"),
          covariate.labels = "N. Daughters",
          dep.var.labels = "AAUW",
          omit = c("nchildren", "congress", "female", "Constant"),
          header = FALSE,
          type = "latex",
          omit.stat=c("LL","ser","f", "rsq"), 
          font.size = "small",
          out = "tabs/table_si_nonbio_ngirls_aauw_by_cong_washington.tex",
          caption = "Estimated Average Treatment Effect Among All MCs Including Non-Biological Children")

data.frame(rbind(boot_w_cong_bio, boot_w_cong_non_bio)) %>% 
  mutate(model = rownames(rbind(boot_w_cong_bio, boot_w_cong_non_bio))) %>%
  select(model, point_estimate, conf_int, t_stat) %>%
  unnest() %>%
  cbind(conf_int_lab = rep(c("ci min", "ci max"), 2)) %>%
  pivot_wider(names_from = conf_int_lab, values_from = conf_int)

