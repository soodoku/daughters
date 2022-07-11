## Data Analysis
## Table 1: Effect of ndaughters over time
## Figure 1: Effect of ndaughters over time among Washington Cohort/Rest
## Does the effect among the rest of the data (minus Washington) overlap the CI of the "original"

### Set working dir
setwd(githubdir)
setwd("daughters/")

### Load libs
library(stargazer)
library(tidyverse)
library(fwildclusterboot)
library(knitr)

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

### Pooled Reg

aauw_ngirls <- lm(aauw_all ~ ngirls + as.factor(congress) + as.factor(nchildren) + female, d)

boot_aauw_ngirls <- boottest(aauw_ngirls, clustid = "id", param = "ngirls", B = 9999)

boot_aauw <- data.frame(rbind(boot_aauw_ngirls)) %>% 
  select(point_estimate, conf_int, N) %>%
  unnest() %>%
  cbind(conf_int_lab = rep(c("ci min", "ci max"), 1)) %>%
  pivot_wider(names_from = conf_int_lab, values_from = conf_int)

kable(boot_aauw, digits = 3)

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
  geom_pointrange(aes(ymin=co-1.96*se, ymax=co + 1.96*se, color = as.factor(ebonya_cohort)), alpha = 0.7, width=.2,
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
