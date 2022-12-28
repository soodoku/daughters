## Data Analysis

### Set working dir
setwd("~/Documents/Github/daughters/")

### Load libs
library(stargazer)
library(foreign)
library(tidyverse)
library(fwildclusterboot)
library(plotly)
library(knitr)

# Set seed
set.seed(1234567)

# Load data
washington <- read.dta("data/washington/basic.dta")

now105 <- lm(nowtot ~ ngirls + as.factor(totchi) + female + white + repub + 
            age + I(age^2) + srvlng + I(srvlng^2) + as.factor(rgroup) + demvote, 
          washington, subset = c(congress == 105))
aauw105 <- lm(aauw ~ ngirls + as.factor(totchi) + female + white + repub + 
            age + I(age^2) + srvlng + I(srvlng^2) + as.factor(rgroup) + demvote, 
          washington, subset = c(congress == 105))
aauw106 <- lm(aauw ~ ngirls + as.factor(totchi) + female + white + repub + 
            age + I(age^2) + srvlng + I(srvlng^2) + as.factor(rgroup) + demvote, 
          washington, subset = c(congress == 106))
aauw107 <- lm(aauw ~ ngirls + as.factor(totchi) + female + white + repub + 
            age + I(age^2) + srvlng + I(srvlng^2) + as.factor(rgroup) + demvote, 
          washington, subset = c(congress == 107))
aauw108 <- lm(aauw ~ ngirls + as.factor(totchi) + female + white + repub + 
            age + I(age^2) + srvlng + I(srvlng^2) + as.factor(rgroup) + demvote, 
          washington, subset = c(congress == 108))

stargazer(now105, aauw105, aauw106, aauw107, aauw108,
          digits = 2, 
          single.row = TRUE,
          omit.stat = c("f", "ser"),
          type = "text", 
          column.labels = c("105th", "105th", "106th", "107th", "108th"))


# Our preferred specification
# Remove childless
washington <- subset(washington, totchi > 0)
washington$aauw <- washington$aauw/100

aauw105 <- lm(aauw ~ ngirls + as.factor(totchi) + female, 
          washington, subset = c(congress == 105))
aauw106 <- lm(aauw ~ ngirls + as.factor(totchi) + female, 
          washington, subset = c(congress == 106))
aauw107 <- lm(aauw ~ ngirls + as.factor(totchi) + female, 
          washington, subset = c(congress == 107))
aauw108 <- lm(aauw ~ ngirls + as.factor(totchi) + female, 
          washington, subset = c(congress == 108))
pooled <- lm(aauw ~ ngirls + as.factor(totchi) + female + as.factor(congress), 
          washington)

# Unique congress members
length(unique(paste0(washington$name, "-", washington$bday)))
length(unique(paste0(washington$name)))
# 517
length(unique(paste0(washington$name, "-", washington$bday, washington$statenam))) #518

stargazer(aauw105, aauw106, aauw107, aauw108, pooled,
          digits = 3, 
          single.row = TRUE,
          omit.stat = c("f", "ser"),
          type = "text", 
          column.labels = c("105th", "106th", "107th", "108th", "Pooled"))

# Get the s.e.
boot_pooled <- boottest(pooled, clustid = "name", param = "ngirls", B = 9999)

boot_aauw <- data.frame(rbind(boot_pooled)) %>% 
  select(point_estimate, conf_int, N) %>%
  unnest(cols = c(point_estimate, conf_int, N)) %>%
  cbind(conf_int_lab = rep(c("ci min", "ci max"), 1)) %>%
  pivot_wider(names_from = conf_int_lab, values_from = conf_int)


