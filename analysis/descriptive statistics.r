# title:Descriptive statistics
# author: Ian Hussey (ian.hussey@ugent.be)
# license: GPLv3+

# Notes: output checked against results returned by JASP


# dependencies ------------------------------------------------------------


library(tidyverse)
library(psych)


# data acquisition --------------------------------------------------------

 
setwd("~/Dropbox/Work/Projects/Analogy/1 analogical learning via IAT with known stimuli/Experiment 1")

data_df <- 
  read.csv("data/processed data/wide D1 scored data.csv") %>%
  filter(IAT_exclude_based_on_fast_trials == FALSE,
         SCIAT_exclude_based_on_fast_trials == FALSE) 


# distribution plots for DVs ----------------------------------------------


# passers only: test IAT D1 condition 1 vs 2
plot(density(data_df$SCIAT_D1[data_df$IAT_condition == "Race IAT"]))
lines(density(data_df$SCIAT_D1[data_df$IAT_condition == "Flowers-Insects IAT"]))


# descriptives ------------------------------------------------------------


# factor variables
gender_counts                 <- data_df %>% count(gender)
condition_counts              <- data_df %>% count(IAT_condition)


# all ps (after exclusions)
descriptives_all_participants <- 
  data_df %>% 
  dplyr::select(age,
                IAT_mean_RT,	
                IAT_perc_acc,
                SCIAT_mean_RT,	
                SCIAT_perc_acc) %>%
  psych::describe(fast = TRUE,  # subset of descriptive stats
                  ranges = FALSE,
                  trim = 0) %>%
  dplyr::select(-vars, -se)

# by condition (after exclusions)
descriptives_by_condition <- 
  data_df %>% 
  dplyr::select(mean_rating,
                IAT_D1,
                SCIAT_D1,
                modern_racism_scale_total) %>%
  psych::describeBy(data_df$IAT_condition,
                    fast = TRUE,  # subset of descriptive stats
                    ranges = FALSE,
                    trim = 0)


# write output to disk ----------------------------------------------------


sink("analysis/descriptive statistics.txt")
cat("\n Gender counts \n")
gender_counts
cat("\n Condition counts \n")
condition_counts
cat("\n")
descriptives_all_participants
cat("\n")
descriptives_by_condition
sink()


