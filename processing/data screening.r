# Screen data

# author: Ian Hussey (ian.hussey@ugent.be)
# license: GPLv3+


# Dependencies ------------------------------------------------------------


library(plyr)
library(tidyverse)
library(data.table)


# Data acquisition and cleaning -------------------------------------------


## Set the working directory
setwd("/Users/Ian/Dropbox/Work/Projects/Analogy/1 analogical learning via IAT with known stimuli/Experiment 1/data/pilot 1/")

# Read all files with the .iqdat extension
files <- list.files(pattern = "\\.csv$")  

# Read these files sequentially into a single data frame
input_df <- dplyr::tbl_df(plyr::rbind.fill(lapply(files, data.table::fread, header = TRUE)))  # tbl_df() requires dplyr, rbind.fill() requires plyr, fread requires data.table

# Make some variable names more transparent
trimmed_df <- 
  input_df %>%
  dplyr::select(subject,
                response,
                trialcode,
                blockcode) %>%
  dplyr::rename(participant = subject,
                task = blockcode) %>%
  dplyr::mutate(participant = as.numeric(participant))

trimmed_df %>% distinct(task)


# complete data per task --------------------------------------------------


## find participants who have data in each task, 
## then for each check if they had data in all the previous tasks
## return those participants who had data in all tasks

##assess what tasks are there - used for the grepl later (as we don't necessarially need to employ all block names)
#tasks <-
#  trimmed_df %>%
#  dplyr::distinct(task)

#separate each tasks and find uniques
participants_with_full_data <- #start with demographics
  trimmed_df %>%
  dplyr::filter(grepl("demographics", task)) %>%  # filter rows where the block_name includes string
  dplyr::distinct(participant)


IAT <-
  trimmed_df %>%
  dplyr::filter(grepl("incompatibletest2", task)) %>%  # filter rows where the block_name includes string
  dplyr::distinct(participant)

participants_with_full_data <- 
  dplyr::semi_join(participants_with_full_data, IAT, by = "participant")


SCIAT <-
  trimmed_df %>%
  dplyr::filter(grepl("incompatibletest", task)) %>%  # filter rows where the block_name includes string
  dplyr::distinct(participant)

participants_with_full_data <- 
  dplyr::semi_join(participants_with_full_data, SCIAT, by = "participant")


ratings <-
  trimmed_df %>%
  dplyr::filter(grepl("ratings", task)) %>%  # filter rows where the block_name includes string
  dplyr::distinct(participant)

participants_with_full_data <- 
  dplyr::semi_join(participants_with_full_data, ratings, by = "participant")


demand_compliance <-
  trimmed_df %>%
  dplyr::filter(grepl("demand_compliance", trialcode)) %>%  # filter rows where the block_name includes string
  dplyr::distinct(participant)

participants_with_full_data <- 
  dplyr::semi_join(participants_with_full_data, demand_compliance, by = "participant")


# modern_racism_scale <-
#   trimmed_df %>%
#   dplyr::filter(grepl("modern_racism_scale", trialcode)) %>%  # filter rows where the block_name includes string
#   dplyr::distinct(participant)
# 
# participants_with_full_data <- 
#   dplyr::semi_join(participants_with_full_data, modern_racism_scale, by = "participant")


# participants with at least partial data
participants_with_at_least_partial_data <-
  trimmed_df %>%
  dplyr::distinct(participant) %>%
  dplyr::mutate(participant_partial = participant) 

# participants with incomplete data
participants_with_incomplete_data <-
  dplyr::anti_join(participants_with_at_least_partial_data, participants_with_full_data, by = "participant")


# make lists of participants ----------------------------------------------


## produce prolific codes so that participants can be credited or rejected

# 1. prolific codes for participants with complete data so that they can be paid
prolific_codes_for_complete_participants <-
  dplyr::inner_join(trimmed_df, participants_with_full_data, by = "participant") %>%
  dplyr::filter(trialcode == "ProlificCode") %>%
  dplyr::select(participant, response) %>%
  dplyr::distinct(participant, .keep_all = TRUE) 

# participants to credit
prolific_codes_for_complete_participants %>% readr::write_csv("processed data/prolific codes - complete data.csv")
# participant with complete data - i.e., the inclusion list
prolific_codes_for_complete_participants %>% select(participant) %>% readr::write_csv("processed data/inclusion list.csv")

# 1.1 N of complete participants
prolific_codes_for_complete_participants %>% dplyr::summarize(participant = n())


# 2. prolific codes for participants with incomplete data so that they can be rejected
prolific_codes_for_incomplete_participants <-
  dplyr::inner_join(trimmed_df, participants_with_incomplete_data, by = "participant") %>%
  dplyr::filter(trialcode == "ProlificCode") %>%
  dplyr::select(participant, response) %>%
  dplyr::distinct(participant, .keep_all = TRUE) 

prolific_codes_for_incomplete_participants %>% readr::write_csv("processed data/prolific codes - incomplete data.csv")

# 2.1 N of incomplete participants
prolific_codes_for_incomplete_participants %>% dplyr::summarize(participant = n())

# 2.2 rejected participants will automatically have their slots reopened and fresh data will be collected.


# 3 Misfits on the prolific site
# the list of participants on prolific will still probably include a number of remaining participants. for example:
# 1. Those who cancelled all tasks (and therefore recorded no inquisit data but nonetheless returned a 
# completion code to prolific). These should be REJECTED. If they are not, they'll be paid automatically after two weeks. 
# 2. Those who entered an incorrect prolific id or did not enter one, but who nonetheless produced legitimate data.  
# These should be APPROVED.

