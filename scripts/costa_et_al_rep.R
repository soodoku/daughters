## Costa et al. replication

### Set working dir
setwd(githubdir)
setwd("daughters/")

### Load libs
library(stargazer)
library(tidyverse)
library(fwildclusterboot)
library(readr)
library(plm)
library(lme4)
library(lmtest)
library(multiwayvcov)

# Set seed
set.seed(1234567)

# Read data
costa <- read_csv("data/costa_et_al/parentsincongress_110-114.csv")
 
# Descriptive
table(costa$Female)
sum(with(costa[!duplicated(costa$ICPSR_ID), ], NumDaughters))
sum(with(costa[!duplicated(costa$ICPSR_ID), ], NumSons))
# sex ratio = 1.06

# Sanity check 
costa$total_children <- rowSums(cbind(costa$NumSons, costa$NumDaughters)) # per row
sum(costa$NumChildren == costa$total_children)
sum(costa$NumChildren == costa$total_children)/nrow(costa)

# Stopping Rule
costa[!duplicated(costa$ICPSR_ID), ] %>% 
	group_by(as.factor(NumChildren)) %>%
	summarize(mean(NumDaughters/NumChildren), n = n())

## Costa Tab 1
with(costa[costa$Female == 0, ], summary(lmer(AAUWrating ~ I(NumDaughters > 0) + NumChildren + I(NumDaughters > 0)*Party + as.factor(Congress) + (1|ICPSR_ID))))
with(costa[costa$Female == 1, ], summary(lmer(AAUWrating ~ I(NumDaughters > 0) + NumChildren + I(NumDaughters > 0)*Party + as.factor(Congress) + (1|ICPSR_ID))))

# Similar to Washington (Washington has more covariates)
with(costa, summary(lmer(AAUWrating ~ NumDaughters + as.factor(NumChildren) + Party + as.factor(Congress) + (1|ICPSR_ID))))

# Regressions
m1 <- plm(AAUWrating ~ 
					I(NumDaughters > 0) + 
					NumChildren + 
					I(NumDaughters > 0)*Party + 
					as.factor(Congress), 
					model = "pooling",
					data = costa[costa$Female == 0, ],
					index = c("ICPSR_ID"))


m2 <- plm(AAUWrating ~ 
					NumDaughters + 
					as.factor(NumChildren) + 
					Party + 
					as.factor(Congress), 
					model = "pooling",
					data = costa[costa$Female == 0, ],
					index = c("ICPSR_ID"))

# Our preferred specification

costa$AAUWrating <- costa$AAUWrating/100

aauw110 <- lm(AAUWrating ~  NumDaughters + as.factor(NumChildren) + Female, costa[costa$Congress == 110, ])

aauw111 <- lm(AAUWrating ~  NumDaughters + as.factor(NumChildren) + Female, costa[costa$Congress == 111, ])

aauw112 <- lm(AAUWrating ~  NumDaughters + as.factor(NumChildren) + Female, costa[costa$Congress == 112, ])

aauw113 <- lm(AAUWrating ~  NumDaughters + as.factor(NumChildren) + Female, costa[costa$Congress == 113, ])

aauw114 <- lm(AAUWrating ~  NumDaughters + as.factor(NumChildren) + Female, costa[costa$Congress == 114, ])

pooled <- lm(AAUWrating ~ NumDaughters + as.factor(NumChildren) + Female + as.factor(Congress), costa)

stargazer(aauw110, aauw111, aauw112, aauw113, aauw114, pooled,
          digits = 3, 
          single.row = TRUE,
          omit.stat = c("f", "ser"),
          type = "text",  
          column.labels = c("110th", "111th", "112th", "113th", "114th", "Pooled"))

