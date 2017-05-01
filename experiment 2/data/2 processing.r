# process data

# author: Ian Hussey (ian.hussey@ugent.be)
# license: GPLv3+

# Notes:
# ProlificCode is used only to pay participants and must be deleted from 
# all data files before raw data is posted online
 

# Dependencies ------------------------------------------------------------


library(plyr)
library(tidyverse)
library(data.table)
library(schoRsch)


# Data acquisition and cleaning -------------------------------------------


# set the working directory
setwd("/Users/Ian/git/Analogical learning via the IAT II/experiment 2/data/processed")

# get screened data
input_df <- read.csv("screened data.csv")

# make some variable names more transparent
cleaned_df <- 
  input_df %>%
  dplyr::mutate(participant = as.numeric(participant),
                block_n = as.numeric(block_n),
                trial_n = as.numeric(trial_n),
                accuracy = as.numeric(accuracy),
                rt = as.numeric(rt), 
                response = as.character(response))


###########################################################################
# wide data
###########################################################################


# demographics and parameters  --------------------------------------------


demo_temp_1_df <-
  cleaned_df %>%
  dplyr::group_by(participant) %>%
  dplyr::filter(grepl("demographics", task),
                item == "age") %>%  # filter rows where the task includes string
  dplyr::rename(age = response) %>%
  dplyr::mutate(age = as.numeric(age)) %>%
  dplyr::select(participant, age) %>% # select only necessary columns
  dplyr::distinct(participant, .keep_all = TRUE)

demo_temp_2_df <-
  cleaned_df %>%
  dplyr::group_by(participant) %>%
  dplyr::filter(grepl("demographics", task),
                item == "gender") %>%  # filter rows where the task includes string
  dplyr::rename(gender = response) %>%
  dplyr::select(participant, gender) %>% # select only necessary columns
  dplyr::distinct(participant, .keep_all = TRUE) %>%
  dplyr::mutate(gender = tolower(gender),  # homogenise gender categories. first make lower case
                gender = ifelse(gender == "f", "female",  # then convert abbreviations
                                ifelse(gender == "m", "male", gender))) %>%
  dplyr::left_join(demo_temp_1_df, by = "participant")

demo_temp_3_df <-
  cleaned_df %>%
  dplyr::filter(grepl("demographics", task)) %>%
  dplyr::distinct(participant) %>%
  dplyr::group_by(participant) %>%
  dplyr::mutate(condition = participant %% 8,
                condition = ifelse(condition == 0, 8, condition),  # convert participant codes to conditions and correct modulus%%8==0 to condition=8
                IAT_condition = ifelse(condition <= 4, "Race IAT", "Flowers-Insects IAT"),
                block_order = ifelse(condition %% 2 == 1, "congruent", "incongruent"),
                task_order = ifelse(condition == 1 | condition == 2 | condition == 5 | condition == 6,
                                    "ratings first", "SCIAT first")) %>%
  dplyr::left_join(demo_temp_2_df, by = "participant")

# amp recognition
demographics_df <-
  cleaned_df %>%
  dplyr::filter(!is.na(amp_recognition_response)) %>%
  dplyr::select(participant, amp_recognition_response) %>%
  dplyr::left_join(demo_temp_3_df, by = "participant")


# ratings -----------------------------------------------------------------


ratings_df <-
  cleaned_df %>%
  dplyr::filter(grepl("ratings", task)) %>%  # filter rows where the item includes string
  dplyr::rename(ratings = response) %>%
  dplyr::select(participant, ratings) %>%
  dplyr::mutate(ratings = as.numeric(ratings)) %>%
  dplyr::group_by(participant) %>%
  dplyr::summarize(mean_rating = round(mean(ratings), 2))


# modern racism scale -----------------------------------------------------


racism_scale_df <-
  cleaned_df %>%
  dplyr::filter(grepl("racism_scale", task)) %>%  # filter rows where the item includes string
  dplyr::select(participant, response) %>%
  dplyr::group_by(participant) %>%
  dplyr::mutate(response = as.integer(response)) %>%
  dplyr::summarize(modern_racism_scale_total = sum(response))


# IAT summary scores ------------------------------------------------------


IAT_summary_stats_df <- 
  cleaned_df %>%
  dplyr::filter(task == "compatibletest1" | 
                  task == "compatibletest2" | 
                  task == "incompatibletest1" |
                  task == "incompatibletest2") %>% # test blocks only
  dplyr::mutate(IAT_too_fast_trial = ifelse(rt < 300, 1, 0)) %>%
  dplyr::group_by(participant) %>%
  dplyr::summarize(IAT_mean_RT = round(mean(rt), 0),
                   IAT_perc_acc = round(sum(accuracy)/n(), 2),
                   IAT_percent_fast_trials = sum(IAT_too_fast_trial)/n()) %>%  # arbitrary number of test block trials
  dplyr::mutate(IAT_exclude_based_on_fast_trials = ifelse(IAT_percent_fast_trials < 0.1, FALSE, TRUE)) %>%  
  dplyr::select(participant,
                IAT_mean_RT,
                IAT_perc_acc,
                IAT_exclude_based_on_fast_trials)


# AMP summary scores ------------------------------------------------------


# AMP_summary_stats_df <- 
#   cleaned_df %>%
#   dplyr::filter(task == "AMP_test") %>% # test block only
#   dplyr::select(participant,
#                 accuracy,
#                 rt,
#                 item) %>%
#   dplyr::mutate(AMP_too_fast_trial = ifelse(rt < 300, 1, 0)) %>%
#   dplyr::group_by(participant) %>%
#   dplyr::summarize(AMP_mean_RT = round(mean(rt), 0),
#                    AMP_perc_acc = round(sum(accuracy)/n(), 2),
#                    AMP_percent_fast_trials = sum(AMP_too_fast_trial)/n()) %>%  # arbitrary number of test block trials
#   dplyr::mutate(AMP_exclude_based_on_fast_trials = ifelse(AMP_percent_fast_trials < 0.1, FALSE, TRUE)) %>%  
#   dplyr::select(participant,
#                 AMP_mean_RT,
#                 AMP_perc_acc,
#                 AMP_exclude_based_on_fast_trials)

## The exclusion criterion of >10% trials <300ms was illconsidered - it didn't take into account the distribution
## of RTs in the AMP and results in a very large proportion of the sample being excluded. 
## Rather than tweak this exclusion criterion post hoc, we simple remove it for the sake of improving attrition rates.
## This change was made prior to analysing the data.

AMP_summary_stats_df <- 
  cleaned_df %>%
  dplyr::filter(task == "AMP_test") %>% # test block only
  dplyr::select(participant,
                accuracy,
                rt,
                item) %>%
  dplyr::group_by(participant) %>%
  dplyr::summarize(AMP_mean_RT = round(mean(rt), 0),
                   AMP_perc_acc = round(sum(accuracy)/n(), 2)) %>%  # arbitrary number of test block trials
  dplyr::select(participant,
                AMP_mean_RT,
                AMP_perc_acc)


# join wide D1 scored data and write to disk ------------------------------


wide_data_df <- 
  plyr::join_all(list(as.data.frame(demographics_df),  # join_all throws a requires input be data.frame error, despite is.data.frame returning TRUE for all members of list. Workaround is to coerce all to DF here. 
                      as.data.frame(racism_scale_df),
                      as.data.frame(IAT_summary_stats_df),
                      as.data.frame(AMP_summary_stats_df)),
                 by = "participant",
                 type = "full") %>%
  dplyr::arrange(participant) %>%
  dplyr::mutate(exclude = ifelse(IAT_exclude_based_on_fast_trials == TRUE, TRUE,
                                 #ifelse(AMP_exclude_based_on_fast_trials == TRUE, TRUE, 
                                 FALSE))

# print N
wide_data_df %>% dplyr::summarize(condition_count = n())

wide_data_df %>% write.csv(file = "processed summary data - wide format.csv", row.names = FALSE)


###########################################################################
# long format data
###########################################################################


# other tasks -------------------------------------------------------------


other_tasks_df <- 
  wide_data_df %>%
  dplyr::select(participant, 
                IAT_condition, 
                block_order, 
                task_order, 
                gender, 
                age, 
                modern_racism_scale_total, 
                amp_recognition_response,
                IAT_exclude_based_on_fast_trials,
                #AMP_exclude_based_on_fast_trials,
                exclude)


# long format AMP ratings -------------------------------------------------


AMP_long_format_df <-
  cleaned_df %>%
  dplyr::filter(task == "AMP_test") %>%
  dplyr::select(participant,
                accuracy,
                rt,
                item) %>%
  dplyr::left_join(other_tasks_df, by = "participant")

AMP_long_format_df %>% write.csv(file = "processed AMP data - long format.csv", row.names = FALSE)


# long format ratings -----------------------------------------------------


ratings_long_format_df <-  
  cleaned_df %>%
  dplyr::filter(task == "ratings") %>%
  dplyr::mutate(rating = as.integer(response)) %>%
  dplyr::select(participant, 
                trial_n,
                rating) %>%
  dplyr::left_join(other_tasks_df, by = "participant")

ratings_long_format_df %>% write.csv(file = "processed ratings data - long format.csv", row.names = FALSE)

