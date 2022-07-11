### Balance Checks

### Set working dir
setwd(githubdir)
setwd("daughters/")

### Load libs
library(stargazer)
library(foreign)
library(tidyverse)
library(knitr)
library(broom)
library(xtable)

# Set seed
set.seed(1234567)

# Balance Among All Number of Children

d <- read_csv("data/final_data_2022_01_05.csv") %>%
  mutate(aauw_all = aauw_all/100,
         aauw_women_all = aauw_women_all/100,
         nominate_dim1 = -nominate_dim1,
         prop_girls = ngirls/nchildren)


## Balance Tests

p <- d %>%
  select(id, ngirls, nchildren) %>%
  unique() %>%
  group_by(nchildren) %>%
  add_tally() %>%
  summarize(prop_daughters = mean(ngirls)/ mean(nchildren),
            n = n) %>%
  ungroup() %>%
  ggplot(aes(as.factor(nchildren), prop_daughters, size = n)) +
  geom_point() + 
  labs(x = "Total Children", y= "Proportion Daughters",
       title = "Proportion of Daughters by Total Children") +
  theme_minimal()

d_unique <- d %>% 
  group_by(id) %>%
  sample_n(1) %>%
  ungroup()

pre_ebonya_unique <- d %>%
  filter(congress < 105) %>%
  group_by(id) %>%
  sample_n(1) %>% 
  ungroup()

ebonya_unique <- d %>%
  filter(congress %in% c(105:108)) %>%
  group_by(id) %>%
  sample_n(1) %>% 
  ungroup()

post_ebonya_unique <- d %>%
  filter(congress > 108) %>%
  group_by(id) %>%
  sample_n(1) %>% 
  ungroup()

## __All Unique MCs__ T-Test Proportion Girls

t.test(d_unique$prop_girls, mu = 0.5)

## __Pre-Ebonya__ T-Test Proportion Girls for All Unique MCs

t.test(pre_ebonya_unique$prop_girls, mu = 0.5)

## __During Ebonya__ T-Test Proportion Girls for All Unique MCs

t.test(ebonya_unique$prop_girls, mu = 0.5)

## __Post-Ebonya__ T-Test Proportion Girls for All Unique MCs

t.test(post_ebonya_unique$prop_girls, mu = 0.5)

# Conditional on Number of Children

## __All Unique MCs__ T-Test Conditional on N Children

# https://stackoverflow.com/questions/51723869/one-sample-t-test-in-r-within-groups

t_all <- d_unique %>%
  filter(nchildren != 12) %>% # not enough mc w/ 12 children
  group_by(nchildren) %>%
  summarise(res = list(tidy(t.test(prop_girls, mu = 0.5)))) %>%
  unnest()

kable(t_all, digits = 3)

t_all$nchildren <- as.integer(t_all$nchildren)
t_all$parameter <- as.integer(t_all$parameter)

t_show <- t_all %>% select(nchildren, estimate, p.value, parameter)
names(t_show) <- c("Number of Children", "Mean Proportion Daughters", "p", "n")

comm <- paste0("\\hline \n \\multicolumn{3}{l}",
           "{\\scriptsize{Note: Results of two-tailed one-sample t-test with null as .5.}}")

print(
        xtable(t_show,
          digits = 3,
          align = "llllc",
          caption = "Proportion of Female Children by Number of Children", 
          label = "tab:balance_prop_female_nchild"), 
        include.rownames = FALSE,
        include.colnames = TRUE, 
        size="\\small", 
        type = "latex", 
        sanitize.text.function = function(x){x},
        caption.placement = "top",
        table.placement = "!htb",
        add.to.row = list(pos = list(nrow(t_show)),command = comm),
        file = "tabs/append_prop_female_by_nchild.tex")

## __Pre-Ebonya__ T-Test Conditional on N Children

t_pre_ebonya <- pre_ebonya_unique %>%
  filter(!nchildren %in% c(9, 10)) %>% 
  group_by(nchildren) %>%
  summarise(res = list(tidy(t.test(prop_girls, mu = 0.5)))) %>%
  unnest()

kable(t_pre_ebonya, digits = 3)


## __During-Ebonya__ T-Test Conditional on N Children

t_during_ebonya <- ebonya_unique %>%
  filter(!nchildren %in% c(10, 12)) %>%
  group_by(nchildren) %>%
  summarise(res = list(tidy(t.test(prop_girls, mu = 0.5)))) %>%
  unnest()

kable(t_during_ebonya, digits = 3)

## __Post-Ebonya__ T-Test Conditional on N Children

t_post_ebonya <- post_ebonya_unique %>%
  filter(!nchildren %in% c(12)) %>%
  group_by(nchildren) %>%
  summarise(res = list(tidy(t.test(prop_girls, mu = 0.5)))) %>%
  unnest()

kable(t_post_ebonya, digits = 3)

# Correlation between Proportion of Daughters and Number of Children

## All Unique MCs

cor(d_unique$prop_girls, d_unique$nchildren)

## __Pre-Ebonya__ All Unique MCs

cor(pre_ebonya_unique$prop_girls, pre_ebonya_unique$nchildren)

## __During-Ebonya__ All Unique MCs

cor(ebonya_unique$prop_girls, ebonya_unique$nchildren)

## __Post-Ebonya__ All Unique MCs

cor(post_ebonya_unique$prop_girls, post_ebonya_unique$nchildren)

# Regressing Proportion Daughters on Party

d_unique <- d_unique %>%
  filter(party %in% c("D", "R")) %>%
  mutate(democrat = ifelse(party == "D", 1, 0))

t_all_party <- d_unique %>%
  #filter(nchildren != 12) %>% # not enough mc w/ 12 children
  group_by(democrat) %>%
  summarise(res = list(tidy(t.test(prop_girls, mu = 0.5)))) %>%
  unnest(cols = c(res))

party_ngirls <- glm(democrat ~ ngirls + as.factor(nchildren), d_unique, family = "binomial")

stargazer(party_ngirls, 
       covariate.labels = c("Number of Daughters", "Constant"),
       label = "tab:party_ngirls",
       dep.var.labels = "p(Democrat)",
       omit = "nchildren",
       title = "Effect of Number of Daughters on Party Controlling for Indicator Variables for Number of Children",
       out   = "tabs/party_ngirls.tex")


# Regressing Proportion Daughters on Covariates

## Region
# via: https://raw.githubusercontent.com/cphalpert/census-regions/master/us%20census%20bureau%20regions%20and%20divisions.csv
regions <- read_csv("data/us_census_bureau_regions_and_divisions.csv")

d_unique_states <- d_unique %>%
  left_join(regions %>% select(`State Code`, Region), by = c("state" = "State Code"))

table(d_unique_states$Region)

summary(lm(ngirls ~ as.factor(Region) + as.factor(nchildren), d_unique_states))


## Female Congress Members

### Number of Female Congress Members

table(d_unique$female)

summary(lm(prop_girls ~ female + as.factor(nchildren), d_unique))
