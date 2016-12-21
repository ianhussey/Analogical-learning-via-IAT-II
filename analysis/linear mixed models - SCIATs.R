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
  read.csv("data/processed data/long SCIAT data.csv") %>%
  mutate(participant = as.factor(participant))

data_outliers_removed <-
  data %>%
  schoRsch::outlier(dv = "rt", 
                    todo="elim", 
                    upper.z = 2.5, 
                    lower.z = -2.5)

# check that what should be factors are indeed factors
sapply(data, class)


# plots -------------------------------------------------------------------


# raw 
plot(density(data$rt), col = "red")
lines(density(data_outliers_removed$rt), col = "blue")
# rescaled
plot(density(data_outliers_removed$rt), col = "blue")
# interactions
with(data_outliers_removed, interaction.plot(block, IAT_condition, rt))


# model 1 ------------------------------------------------------------------


# frequentist mixed linear effects model with participant as a random effect
# and racism as covariate

# implemented using afex on top of lmer, to produce *p values*
# NB production of p values (over LRs etc) is contentious, but cite the
# following as recent evidence for the use of kenward roger method estiamtion: 
# http://link.springer.com/article/10.3758%2Fs13428-016-0809-y

# No effect sizes are produced due to contention over how to use the random
# factor error. See 
# http://stats.stackexchange.com/questions/95054/how-to-get-an-overall-p-value-and-effect-size-for-a-categorical-factor-in-a-mi

model_1 <- afex::mixed(rt ~ block * IAT_condition + modern_racism_scale_total + (1 | participant), # entering participant as a random effect acknowledges the non-independence of the multiple data points for each participant
                       contrasts = TRUE,  # set to true by default, but worth emphasising here that we're using effect coding not dummy coding.
                       data = data_outliers_removed,
                       type = 3,  # sum of squares
                       method = "KR",  # Kenward-Roger method of approximation of df for p values. Parametic bootstrapping ("PB") and liklihood ratio tests ("LR") also available.
                       progress = TRUE, 
                       return = "mixed")

save(model_1, file = "analysis/model_1_lmm_freq_sciats.RData")
#load(model_1, file = "analysis/model_1_lmm_freq_sciats.RData")

summary(model_1)
print(model_1)  # same as using anova() here

# write to disk
sink("analysis/1 frequentist linear mixed effects model - sciats.txt")
summary(model_1)
print(model_1)  # same as using anova() here
sink()


# model 2 ------------------------------------------------------------------


# Bayes factors mixed linear effects model with participant as a random effect
# and racism as covariate

model_2 <- generalTestBF(rt ~ block * IAT_condition + modern_racism_scale_total + participant, 
                         whichRandom = "participant",  # random factors
                         data = data_outliers_removed,
                         rscaleFixed = "medium",  # default 
                         rscaleCont = "medium",  # default
                         rscaleRandom = "nuisance",  # default
                         multicore = TRUE) 

save(model_2, file = "analysis/model_2_lmm_BF_sciats.RData")
#load(model_2, file = "analysis/model_2_lmm_BF_sciats.RData")

# all BF models
model_2

# BFs are transitive, so the contribution of the interaction is calculated by
# dividing the full model by the model without the interaction.
model_2["block + IAT_condition + block:IAT_condition + modern_racism_scale_total + participant"] /
  model_2["block + modern_racism_scale_total + participant"]

# write to disk
sink("analysis/2 BF linear mixed effects model - sciats.txt")
cat("FULL MODEL \n\n")
model_2
cat("\n\ninteraction after controlling for participant (random) and racism (fixed)\n\n")
model_2["block + IAT_condition + block:IAT_condition + modern_racism_scale_total + participant"] /
  model_2["block + modern_racism_scale_total + participant"]
sink()

