# title: RDI plots (raw data, distribution and inferential information)
# author: Ian Hussey (ian.hussey@ugent.be)
# license: GPLv3+


# Dependencies ------------------------------------------------------------


library(tidyverse)
library(yarrr)
library(lme4)


# Data acquisition and processing -----------------------------------------


setwd("/Users/Ian/Dropbox/Work/Projects/Analogy/1 analogical learning via IAT with known stimuli/Experiment 1/")

data <- read.csv("data/processed data/long ratings data.csv")


# control ratings for self reported racism --------------------------------


# model ratings controlling for modern racism scale total score with participant as random factor
model_1 <- lmer(rating ~ modern_racism_scale_total + (1 | participant), 
                data = data)

# add these residuals to the original data frame
data <- 
  data %>%
  mutate(ratings_residuals = residuals(model_1, type = "response"))


# plot --------------------------------------------------------------------


# plot residuals. 
# I.e., plot ratings after controlling for self reported racism, 
# and acknowledging non independence of ratings
RDI_plot <- 
  pirateplot(formula = ratings_residuals ~ IAT_condition,
             data = data,
             main = "Self reports",
             ylab = "Residuals",
             xlab = "Condition",
             theme = 1,
             pal = "#526273",
             bean.b.o = 1,
             point.o = .2,
             bar.o = 1,
             line.o = 1,
             inf.f.o = 1,
             inf = "ci",
             inf.f.col = c("#834339", "#80C48E"),
             bean.f.col = c("#834339", "#80C48E"))


