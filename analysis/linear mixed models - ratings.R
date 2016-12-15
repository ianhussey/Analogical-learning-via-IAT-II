# title: linear mixed models - frequentist and bayes factors

# author: Ian Hussey (ian.hussey@ugent.be)
# license: GPLv3+ 


# dependencies ------------------------------------------------------------


library(ez)
library(tidyverse)
library(car)
library(schoRsch)
library(afex)  # stacks on top of lmer for p values, eta2 etc
library(BayesFactor)


# data acquisition --------------------------------------------------------


setwd("~/Dropbox/Work/Projects/Analogy/1 analogical learning via IAT with known stimuli/Experiment 1/")

data <- 
  read.csv("data/processed data/long ratings data.csv") %>%
  mutate(participant = as.factor(participant))

data_demand_compliance_removed <-
  data %>%
  filter(demand_compliance_boolean == FALSE)

# check that what should be factors are indeed factors
sapply(data, class)


# plots -------------------------------------------------------------------


# raw 
plot(density(data$rating), col = "red")
lines(density(data_outliers_removed$rating), col = "blue")
# rescaled
plot(density(data_outliers_removed$rating), col = "blue")


# model 5 ------------------------------------------------------------------


# frequentist mixed linear effects model with participant as a random effect
# and racism as covariate

# implemented using afex on top of lmer, to produce *p values*
# NB production of p values (over LRs etc) is contentious, but cite the
# following as recent evidence for the use of kenward roger method estiamtion: 
# http://link.springer.com/article/10.3758%2Fs13428-016-0809-y

# No effect sizes are produced due to contention over how to use the random
# factor error. See 
# http://stats.stackexchange.com/questions/95054/how-to-get-an-overall-p-value-and-effect-size-for-a-categorical-factor-in-a-mi

model_5 <- afex::mixed(rating ~ IAT_condition + modern_racism_scale_total + (1 | participant), # entering participant as a random effect acknowledges the non-independence of the multiple data points for each participant
                       contrasts = TRUE,  # set to true by default, but worth emphasising here that we're using effect coding not dummy coding.
                       data = data,
                       type = 3,  # sum of squares
                       method = "KR",  # Kenward-Roger method of approximation of df for p values. Parametic bootstrapping ("PB") and liklihood ratio tests ("LR") also available.
                       progress = TRUE, 
                       return = "mixed")

save(model_5, file = "analysis/model_5_lmm_freq_ratings.RData")
#load(model_5, file = "analysis/model_5_lmm_freq_ratings.RData")

summary(model_5)
print(model_5)  # same as using anova() here

# write to disk
sink("analysis/5 frequentist linear mixed effects model - ratings.txt")
summary(model_5)
print(model_5)  # same as using anova() here
sink()


# model 6 ------------------------------------------------------------------


# Bayes factors mixed linear effects model with participant as a random effect
# and racism as covariate

model_6 <- generalTestBF(rating ~ IAT_condition + modern_racism_scale_total + participant, 
                         whichRandom = "participant",  # random factors
                         data = data,
                         rscaleFixed = "medium",  # default 
                         rscaleCont = "medium",  # default
                         rscaleRandom = "nuisance",  # default
                         multicore = TRUE) 

save(model_6, file = "analysis/model_6_lmm_BF_ratings.RData")
#load(model_6, file = "analysis/model_6_lmm_BF_ratings.RData")

# all BF models
model_6

# BFs are transitive, so the contribution of the interaction is calculated by
# dividing the full model by the model without the interaction.
model_6["IAT_condition + modern_racism_scale_total + participant"] /
  model_6["modern_racism_scale_total + participant"]

# write to disk
sink("analysis/6 BF linear mixed effects model - ratings.txt")
cat("FULL MODEL \n\n")
model_6
cat("\n\ncondition after controlling for participant (random) and racism (fixed)\n\n")
model_6["IAT_condition + modern_racism_scale_total + participant"] /
  model_6["modern_racism_scale_total + participant"]
sink()


# model 7 ------------------------------------------------------------------


# frequentist mixed linear effects model with participant as a random effect
# and racism as covariate
# demand compliant participants removed

model_7 <- afex::mixed(rating ~ IAT_condition + modern_racism_scale_total + (1 | participant), # entering participant as a random effect acknowledges the non-independence of the multiple data points for each participant
                       contrasts = TRUE,  # set to true by default, but worth emphasising here that we're using effect coding not dummy coding.
                       data = data_demand_compliance_removed,
                       type = 3,  # sum of squares
                       method = "KR",  # Kenward-Roger method of approximation of df for p values. Parametic bootstrapping ("PB") and liklihood ratio tests ("LR") also available.
                       progress = TRUE, 
                       return = "mixed")

save(model_7, file = "analysis/model_7_lmm_freq_ratings.RData")
#load(model_7, file = "analysis/model_7_lmm_freq_ratings.RData")

summary(model_7)
print(model_7)  # same as using anova() here

# write to disk
sink("analysis/7 frequentist linear mixed effects model - ratings - demand compliant removed.txt")
summary(model_7)
print(model_7)  # same as using anova() here
sink()


# model 8 ------------------------------------------------------------------


# Bayes factors mixed linear effects model with participant as a random effect
# and racism as covariate
# demand compliant participants removed

model_8 <- generalTestBF(rating ~ IAT_condition + modern_racism_scale_total + participant, 
                         whichRandom = "participant",  # random factors
                         data = data_demand_compliance_removed,
                         rscaleFixed = "medium",  # default 
                         rscaleCont = "medium",  # default
                         rscaleRandom = "nuisance",  # default
                         multicore = TRUE) 

save(model_8, file = "analysis/model_8_lmm_BF_ratings.RData")
#load(model_8, file = "analysis/model_8_lmm_BF_ratings.RData")

# all BF models
model_8

# BFs are transitive, so the contribution of the interaction is calculated by
# dividing the full model by the model without the interaction.
model_8["IAT_condition + participant"] /
  model_8["participant"]

# write to disk
sink("analysis/8 BF linear mixed effects model - ratings - demand compliance removed.txt")
cat("FULL MODEL \n\n")
model_8
cat("\n\ncondition after controlling for participant (random) and racism (fixed)\n\n")
model_8["IAT_condition + modern_racism_scale_total + participant"] /
  model_8["modern_racism_scale_total + participant"]
sink()

