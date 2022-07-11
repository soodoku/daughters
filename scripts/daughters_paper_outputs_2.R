## Data Analysis
# 1. Pooled Regression
# 2. 


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

# Load dat

nominate_scores <- read_csv("data/voteview_congress_members.csv") %>%
  filter(chamber == "House") %>%
  select(id = bioguide_id, congress, nokken_poole_dim1) 

# AAUW Scores have been divided by 100 for comparability. AAUW ranges from 0 to 1.
# Nominate Scores have been reversed for comparability also, so **-1 = Conservative** and **1 = Liberal** and then rescaled to 0 to 1

d <- read_csv("data/final_data_2022_01_05.csv") %>%
  left_join(nominate_scores, by = c("id", "congress")) %>%
  mutate(aauw_all = aauw_all/100,
         aauw_women_all = aauw_women_all/100,
         nom_reverse = - nominate_dim1,
         nominate_dim1 = (nom_reverse - min(nom_reverse, na.rm = T))/(max(nom_reverse, na.rm = T) - min(nom_reverse, na.rm = T)),
         nok_reverse = - nokken_poole_dim1,
         nokken_poole_dim1 = (nok_reverse - min(nok_reverse, na.rm = T))/(max(nok_reverse, na.rm = T) - min(nok_reverse, na.rm = T)),
         prop_girls = ngirls/nchildren)

## SI 7
## AAUW Over Time

d %>%
  filter(party != "I") %>%  # too few I to use geom_smooth
  mutate(party = ifelse(party == "D", "Democratic", "Republican")) %>%
  ggplot(aes(congress, aauw_all, color = party)) +
  geom_point(position = "jitter", alpha = 0.3, size = 0.7) + 
  geom_smooth() +
  scale_color_manual(values = c("Democratic" = "blue", "Republican" = "red")) +
  theme_minimal() +
  labs(title = "AAUW by Congress", x = "Congress", y = "AAUW", color = "") 

ggsave("figs/si_aauw_over_time.pdf")

## Correlation between AAUW & DW Nom

cor(d$aauw_all, d$nokken_poole_dim1, use = "na.or.complete")
cor(d$aauw_all, d$nominate_dim1, use = "na.or.complete")

## Pooled regression w/ Wild Cluster Bootstrap

### AAUW

aauw_any    <- lm(aauw_all ~ anygirls + as.factor(congress) + as.factor(nchildren) + female, d)
aauw_ngirls <- lm(aauw_all ~ ngirls + as.factor(congress) + as.factor(nchildren) + female, d)
aauw_prop   <- lm(aauw_all ~ prop_girls + as.factor(congress) + as.factor(nchildren) + female, d)

boot_aauw_any <- boottest(aauw_any, clustid = "id", param = "anygirls", B = 9999)
boot_aauw_ngirls <- boottest(aauw_ngirls, clustid = "id", param = "ngirls", B = 9999)
boot_aauw_prop <- boottest(aauw_prop, clustid = "id", param = "prop_girls", B = 9999)

boot_aauw <- data.frame(rbind(boot_aauw_any, boot_aauw_ngirls, boot_aauw_prop)) %>% 
  mutate(model = rownames(rbind(boot_aauw_any, boot_aauw_ngirls, boot_aauw_prop))) %>%
  select(model, point_estimate, conf_int, t_stat) %>%
  unnest() %>%
  cbind(conf_int_lab = rep(c("ci min", "ci max"), 3)) %>%
  pivot_wider(names_from = conf_int_lab, values_from = conf_int)

kable(boot_aauw, digits = 3)

### Alternate Outcome Variables
### AAUW Explicitly Women's Issues

aauw_women_any    <- lm(aauw_women_all ~ anygirls + as.factor(congress) + as.factor(nchildren) + female, d)
aauw_women_ngirls <- lm(aauw_women_all ~ ngirls + as.factor(congress) + as.factor(nchildren) + female, d)
aauw_women_prop   <- lm(aauw_women_all ~ prop_girls + as.factor(congress) + as.factor(nchildren) + female, d)

boot_aauw_women_any <- boottest(aauw_women_any, clustid = "id", param = "anygirls", B = 9999)
boot_aauw_women_ngirls <- boottest(aauw_women_ngirls, clustid = "id", param = "ngirls", B = 9999)
boot_aauw_women_prop <- boottest(aauw_women_prop, clustid = "id", param = "prop_girls", B = 9999)

boot_aauw_women <- data.frame(rbind(boot_aauw_women_any, boot_aauw_women_ngirls, boot_aauw_women_prop)) %>% 
  mutate(model = rownames(rbind(boot_aauw_women_any, boot_aauw_women_ngirls, boot_aauw_women_prop))) %>%
  select(model, point_estimate, conf_int, t_stat) %>%
  unnest() %>%
  cbind(conf_int_lab = rep(c("ci min", "ci max"), 3)) %>%
  pivot_wider(names_from = conf_int_lab, values_from = conf_int)

kable(boot_aauw_women, digits = 3)

#### AAUW Voting (Not including cosponsorships), Ngirls

aauw_all_ngirls <- lm(aauw_women_voting/100 ~ ngirls + as.factor(congress) + as.factor(nchildren) + female, d)

### Nominate Scores (Where 0 = Conservative, 1 = Liberal)

nom_any    <- lm(nominate_dim1 ~ anygirls + as.factor(congress) + as.factor(nchildren) + female, d)
nom_ngirls <- lm(nominate_dim1 ~ ngirls + as.factor(congress) + as.factor(nchildren) + female, d)
nom_prop   <- lm(nominate_dim1 ~ prop_girls + as.factor(congress) + as.factor(nchildren) + female, d)

boot_nom_any <- boottest(nom_any, clustid = "id", param = "anygirls", B = 9999)
boot_nom_ngirls <- boottest(nom_ngirls, clustid = "id", param = "ngirls", B = 9999)
boot_nom_prop <- boottest(nom_prop, clustid = "id", param = "prop_girls", B = 9999)

boot_nominate <- data.frame(rbind(boot_nom_any, boot_nom_ngirls, boot_nom_prop)) %>% 
  mutate(model = rownames(rbind(boot_nom_any, boot_nom_ngirls, boot_nom_prop))) %>%
  select(model, point_estimate, conf_int, t_stat) %>%
  unnest() %>%
  cbind(conf_int_lab = rep(c("ci min", "ci max"), 3)) %>%
  pivot_wider(names_from = conf_int_lab, values_from = conf_int)

kable(boot_nominate, digits = 3)

## Visualize Estimated Effect

### Compare AAUW and Nominate Over Congresses

fitted_anygirls <- d %>% 
  group_by(congress) %>% 
  do(aauw_model = lm(aauw_all ~ anygirls + as.factor(nchildren) + female, data = .),
     nominate_model = lm(nominate_dim1 ~ anygirls + as.factor(nchildren) + female, data = .)) %>%
  mutate(co_aauw = coef(aauw_model)["anygirls"],
         se_aauw = coef(summary(aauw_model))["anygirls", "Std. Error"],
         co_nominate = coef(nominate_model)["anygirls"],
         se_nominate = coef(summary(nominate_model))["anygirls", "Std. Error"])

fitted_ngirls <- d %>% 
  group_by(congress) %>% 
  do(aauw_model = lm(aauw_all ~ ngirls + as.factor(nchildren) + female, data = .),
     nominate_model = lm(nominate_dim1 ~ ngirls + as.factor(nchildren) + female, data = .)) %>%
  mutate(co_aauw = coef(aauw_model)["ngirls"],
         se_aauw = coef(summary(aauw_model))["ngirls", "Std. Error"],
         co_nominate = coef(nominate_model)["ngirls"],
         se_nominate = coef(summary(nominate_model))["ngirls", "Std. Error"])

fitted_propgirls <- d %>% 
  group_by(congress) %>% 
  do(aauw_model = lm(aauw_all ~ prop_girls + as.factor(nchildren) + female, data = .),
     nominate_model = lm(nominate_dim1 ~ prop_girls + as.factor(nchildren) + female, data = .)) %>%
  mutate(co_aauw = coef(aauw_model)["prop_girls"],
         se_aauw = coef(summary(aauw_model))["prop_girls", "Std. Error"],
         co_nominate = coef(nominate_model)["prop_girls"],
         se_nominate = coef(summary(nominate_model))["prop_girls", "Std. Error"])

fitted_anygirls %>%
  pivot_longer(cols = c(co_aauw:se_nominate), names_to = c("type", "dep_var"), names_sep = "_") %>%
  pivot_wider(names_from = type) %>%
  ggplot(aes(congress, co)) +
  geom_rect(aes(xmin=105, xmax=108, ymin=-Inf, ymax=Inf), fill="lightgrey", alpha=0.5) +
  geom_rect(aes(xmin=110, xmax=114, ymin=-Inf, ymax=Inf), fill="lightgrey", alpha=0.5) +
  geom_line(aes(color = dep_var)) +
  #geom_point(aes(size = se, color = dep_var)) +
  geom_pointrange(aes(ymin=co-se, ymax=co + se, color = dep_var), width=.2,
                 position=position_dodge(0.05)) + 
  labs(title = "Any Daughters",
       x = "Congress", y = "DV Value", size = "SE", color = "Dependent Variable") +
  theme_minimal() +
  theme(legend.position = "bottom")

fitted_ngirls %>%
  pivot_longer(cols = c(co_aauw:se_nominate), names_to = c("type", "dep_var"), names_sep = "_") %>%
  pivot_wider(names_from = type) %>%
  ggplot(aes(congress, co)) +
  geom_rect(aes(xmin=105, xmax=108, ymin=-Inf, ymax=Inf), fill="lightgrey", alpha=0.5) +
  geom_rect(aes(xmin=110, xmax=114, ymin=-Inf, ymax=Inf), fill="lightgrey", alpha=0.5) +
  geom_line(aes(color = dep_var)) +
  geom_point(aes(size = se, color = dep_var)) +
  labs(title = "Number of Daughters",
       x = "Congress", y = "DV Value", size = "SE", color = "Dependent Variable") +
  theme_minimal() +
  theme(legend.position = "bottom")

fitted_propgirls %>%
  pivot_longer(cols = c(co_aauw:se_nominate), names_to = c("type", "dep_var"), names_sep = "_") %>%
  pivot_wider(names_from = type) %>%
  ggplot(aes(congress, co)) +
  geom_rect(aes(xmin=105, xmax=108, ymin=-Inf, ymax=Inf), fill="lightgrey", alpha=0.5) +
  geom_rect(aes(xmin=110, xmax=114, ymin=-Inf, ymax=Inf), fill="lightgrey", alpha=0.5) +
  geom_line(aes(color = dep_var)) +
  geom_point(aes(size = se, color = dep_var)) +
  labs(title = "Proportion Daughters",
       x = "Congress", y = "DV Value", size = "SE", color = "Dependent Variable") +
  theme_minimal() +
  theme(legend.position = "bottom")


## AAUW Scores on Any Daughters

congress <- as.character(c(97:116))

stargazer(fitted_anygirls$aauw_model,
          column.labels = congress,
          omit = "nchildren",
          type = "html",
          omit.stat=c("LL","ser","f"))

## AAUW Scores on Number of Daughters

stargazer(fitted_ngirls$aauw_model,
          column.labels = congress,
          omit = "nchildren",
          type = "html",
          omit.stat=c("LL","ser","f"))

## AAUW Scores on Proportion of Daughters

stargazer(fitted_propgirls$aauw_model,
          column.labels = congress,
          omit = "nchildren",
          header = FALSE,
          type = "html",
          omit.stat=c("LL","ser","f"))

### Nominate Scores on Any Daughters

stargazer(fitted_anygirls$nominate_model,
          column.labels = congress,
          omit = "nchildren",
          header = FALSE,
          type = "html",
          omit.stat=c("LL","ser","f"))

### Nominate Scores on Number of Daughters

stargazer(fitted_ngirls$nominate_model,
          column.labels = congress,
          omit = "nchildren",
          header = FALSE,
          type = "html",
          omit.stat=c("LL","ser","f"))

### Nominate Scores on Proportion of Daughters

stargazer(fitted_propgirls$nominate_model,
          column.labels = congress,
          omit = "nchildren",
          header = FALSE,
          type = "html",
          omit.stat=c("LL","ser","f"))

### Limiting to women's issues

aauw_women_ngirls <- lm(aauw_women_all ~ ngirls + as.factor(congress) + as.factor(nchildren) + female, d)
fitted_ngirls_womens <- d %>% 
  group_by(congress) %>% 
  do(aauw_model = lm(aauw_women_all ~ ngirls + as.factor(nchildren) + female, data = .)) %>%
  mutate(co_aauw = coef(aauw_model)["ngirls"],
         se_aauw = coef(summary(aauw_model))["ngirls", "Std. Error"])

stargazer(fitted_ngirls_womens$aauw_model,
          column.labels = congress,
          omit = c("nchildren", "female", "Constant"),
          out = "tabs/womens_issues_aauw.tex",
          omit.stat=c("LL","ser","f"))


