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
setwd("/Users/Ian/git/Analogical learning via the IAT II/experiment 1/data/processed")

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
# wide data from here
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

demographics_df <-
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


# IAT D1 scores -----------------------------------------------------------


# select relevant data
IAT_data_df <-  
  cleaned_df %>%
  dplyr::filter(task == "compatibletest1" | 
                  task == "compatibletest2" | 
                  task == "incompatibletest1" |
                  task == "incompatibletest2",  # Test IAT only, and test blocks only
                rt <= 10000)  # rts less than 10,000 only            

# D1 and mean rt
IAT_D1_df <-
  IAT_data_df %>%
  dplyr::group_by(participant) %>%
  dplyr::summarize(rt_mean_compatible_1 = mean(rt[task == "compatibletest1"], na.rm = TRUE),
                   rt_mean_incompatible_1 = mean(rt[task == "incompatibletest1"], na.rm = TRUE),
                   rt_mean_compatible_2 = mean(rt[task == "compatibletest2"], na.rm = TRUE),
                   rt_mean_incompatible_2 = mean(rt[task == "incompatibletest2"], na.rm = TRUE),
                   rt_sd_1 = sd(rt[task == "compatibletest1" | task == "incompatibletest1"]),
                   rt_sd_2 = sd(rt[task == "compatibletest2" | task == "incompatibletest2"])) %>%
  dplyr::mutate(diff_1 = rt_mean_incompatible_1 - rt_mean_compatible_1, # this is effectively a rowwise() calculation as we have group_by() participant and then summarize()'d. rowwise() not included for brevity.
                diff_2 = rt_mean_incompatible_2 - rt_mean_compatible_2,
                D1a = diff_1 / rt_sd_1,
                D1b = diff_2 / rt_sd_2,
                IAT_D1 = round((D1a + D1b)/2, 3)) %>%  # rounding for output simplicity is done only after D1 score calculation
  dplyr::select(participant, 
                IAT_D1)

# calculate % acc and % fast trials from test block data
IAT_data_df$too_fast_trial <- ifelse(IAT_data_df$rt < 300, 1, 0)  # add new column that records if RT < 300ms.

IAT_summary_stats_df <- 
  IAT_data_df %>%
  dplyr::group_by(participant) %>%
  dplyr::summarize(IAT_mean_RT = round(mean(rt), 0),
                   IAT_perc_acc = round(sum(accuracy)/n(), 2),
                   percent_fast_trials = sum(too_fast_trial)/n()) %>%  # arbitrary number of test block trials
  dplyr::mutate(IAT_exclude_based_on_fast_trials = ifelse(percent_fast_trials < 0.1, FALSE, TRUE)) %>%  
  dplyr::select(participant,
                IAT_mean_RT,
                IAT_perc_acc,
                IAT_exclude_based_on_fast_trials)


# SC-IAT D1 scores --------------------------------------------------------


# select relevant data
SCIAT_data_df <-  
  cleaned_df %>%
  dplyr::filter(task == "compatibletest" | 
                  task == "incompatibletest",
                rt <= 10000)  # rts less than 10,000 only            

# D1 and mean rt
SCIAT_D1_df <-
  SCIAT_data_df %>%
  dplyr::group_by(participant) %>%
  dplyr::summarize(rt_mean_compatible = mean(rt[task == "compatibletest"], na.rm = TRUE),
                   rt_mean_incompatible = mean(rt[task == "incompatibletest"], na.rm = TRUE),
                   rt_sd = sd(rt[task == "compatibletest" | task == "incompatibletest"])) %>%
  dplyr::mutate(diff = rt_mean_incompatible - rt_mean_compatible, # this is effectively a rowwise() calculation as we have group_by() participant and then summarize()'d. rowwise() not included for brevity.
                SCIAT_D1 = round(diff / rt_sd, 3)) %>%
  dplyr::select(participant, 
                SCIAT_D1)

# calculate % acc and % fast trials from test block data
SCIAT_data_df$too_fast_trial <- ifelse(SCIAT_data_df$rt < 300, 1, 0)  # add new column that records if RT < 300ms.

SCIAT_summary_stats_df <- 
  SCIAT_data_df %>%
  dplyr::group_by(participant) %>%
  dplyr::summarize(SCIAT_mean_RT = round(mean(rt), 0),
                   SCIAT_perc_acc = round(sum(accuracy)/n(), 2),
                   percent_fast_trials = sum(too_fast_trial)/n()) %>%  # arbitrary number of test block trials
  dplyr::mutate(SCIAT_exclude_based_on_fast_trials = ifelse(percent_fast_trials < 0.1, FALSE, TRUE)) %>%  
  dplyr::select(participant,
                SCIAT_mean_RT,
                SCIAT_perc_acc,
                SCIAT_exclude_based_on_fast_trials)


# join wide D1 scored data and write to disk ------------------------------


wide_D1_scored_data_df <- 
  plyr::join_all(list(as.data.frame(demographics_df),  # join_all throws a requires input be data.frame error, despite is.data.frame returning TRUE for all members of list. Workaround is to coerce all to DF here. 
                      as.data.frame(ratings_df),
                      as.data.frame(IAT_D1_df),
                      as.data.frame(IAT_summary_stats_df),
                      as.data.frame(SCIAT_D1_df),
                      as.data.frame(SCIAT_summary_stats_df),
                      as.data.frame(racism_scale_df)),
                 by = "participant",
                 type = "full") %>%
  dplyr::arrange(participant)

# print N
wide_D1_scored_data_df %>% dplyr::summarize(condition_count = n())

wide_D1_scored_data_df %>% write.csv(file = "processed summary data - wide format.csv", row.names = FALSE)



###########################################################################
# long format data on raw rts/ratings from here
###########################################################################



# other tasks -------------------------------------------------------------


other_tasks_df <- 
  wide_D1_scored_data_df %>%
  dplyr::select(participant, IAT_condition, block_order, 
                task_order, gender, age, modern_racism_scale_total, IAT_exclude_based_on_fast_trials, SCIAT_exclude_based_on_fast_trials)


# long format SC-IAT rts --------------------------------------------------


SCIAT_long_format_df <-  
  cleaned_df %>%
  dplyr::filter(task == "compatibletest" | 
                  task == "incompatibletest") %>%
  dplyr::mutate(block = ifelse(task == "compatibletest", "compatible", "incompatible")) %>%
  dplyr::select(participant, 
                block,
                trial_n,
                rt,
                accuracy) %>%
  dplyr::left_join(other_tasks_df, by = "participant")

SCIAT_long_format_df %>% write.csv(file = "processed SCIAT data - long format.csv", row.names = FALSE)


# long format ratings rts -------------------------------------------------


ratings_long_format_df <-  
  cleaned_df %>%
  dplyr::filter(task == "ratings") %>%
  dplyr::mutate(rating = as.integer(response)) %>%
  dplyr::select(participant, 
                trial_n,
                rating) %>%
  dplyr::left_join(other_tasks_df, by = "participant")

ratings_long_format_df %>% write.csv(file = "processed ratings data - long format.csv", row.names = FALSE)

