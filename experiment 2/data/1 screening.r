# Screen data

# author: Ian Hussey (ian.hussey@ugent.be)
# license: GPLv3+


# Dependencies ------------------------------------------------------------


library(plyr)
library(tidyverse)
library(data.table)


# Data acquisition and cleaning -------------------------------------------


## Set the working directory
setwd("/Users/Ian/git/Analogical learning via the IAT II/experiment 2/data/raw")

# Read all files with the .iqdat extension
files <- list.files(pattern = "\\.csv$")  

# Read these files sequentially into a single data frame
input_df <- dplyr::tbl_df(plyr::rbind.fill(lapply(files, data.table::fread, header = TRUE)))  # tbl_df() requires dplyr, rbind.fill() requires plyr, fread requires data.table

setwd("/Users/Ian/git/Analogical learning via the IAT II/experiment 2/data/processed")

# Make some variable names more transparent
trimmed_df <- 
  input_df %>%
  dplyr::select(subject,
                response,
                trialcode,
                blockcode,
                amp_recognition_response) %>%
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

# no base funciton for mode, so define one
modal_value <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#separate each tasks and find uniques
participants_with_full_data <- #start with demographics
  trimmed_df %>%
  dplyr::filter(grepl("demographics", task)) %>%  # filter rows where the block_name includes string
  dplyr::distinct(participant)

IAT <-
  trimmed_df %>%
  dplyr::filter(grepl("compatibletest", task)) %>%  # filter rows where the block_name includes string
  dplyr::group_by(participant) %>%
  dplyr::summarize(IAT_rows = n()) %>%  # count the number of trials per participant
  dplyr::ungroup() %>%
  dplyr::mutate(modal_IAT_rows = modal_value(IAT_rows)) %>%  # find modal n of trials
  dplyr::rowwise() %>%
  dplyr::filter(IAT_rows == modal_IAT_rows) %>% # if modal n != n then data is missing or participants has duplicate performance.
  dplyr::select(-modal_IAT_rows)

participants_with_full_data <- 
  dplyr::semi_join(participants_with_full_data, IAT, by = "participant")

AMP <-
  trimmed_df %>%
  dplyr::filter(grepl("AMP_test", task)) %>%  # filter rows where the block_name includes string
  dplyr::group_by(participant) %>%
  dplyr::summarize(AMP_rows = n()) %>%  # count the number of trials per participant
  dplyr::ungroup() %>%
  dplyr::mutate(modal_AMP_rows = modal_value(AMP_rows)) %>%  # find modal n of trials
  dplyr::rowwise() %>%
  dplyr::filter(AMP_rows == modal_AMP_rows) %>% # if modal n != n then data is missing or participants has duplicate performance.
  dplyr::select(-modal_AMP_rows)

participants_with_full_data <-
  dplyr::semi_join(participants_with_full_data, AMP, by = "participant")

ratings <-
  trimmed_df %>%
  dplyr::filter(grepl("ratings", task)) %>%  # filter rows where the block_name includes string
  dplyr::group_by(participant) %>%
  dplyr::summarize(ratings_rows = n()) %>%  # count the number of trials per participant
  dplyr::ungroup() %>%
  dplyr::mutate(modal_ratings_rows = modal_value(ratings_rows)) %>%  # find modal n of trials
  dplyr::rowwise() %>%
  dplyr::filter(ratings_rows == modal_ratings_rows) %>% # if modal n != n then data is missing or participants has duplicate performance.
  dplyr::select(-modal_ratings_rows)

participants_with_full_data <- 
  dplyr::semi_join(participants_with_full_data, ratings, by = "participant")


modern_racism_scale <-
  trimmed_df %>%
  dplyr::filter(grepl("racism_scale", task)) %>%  # filter rows where the block_name includes string
  dplyr::group_by(participant) %>%
  dplyr::summarize(racism_scale_rows = n()) %>%  # count the number of trials per participant
  dplyr::ungroup() %>%
  dplyr::mutate(modal_racism_scale_rows = modal_value(racism_scale_rows)) %>%  # find modal n of trials
  dplyr::rowwise() %>%
  dplyr::filter(racism_scale_rows == modal_racism_scale_rows) %>% # if modal n != n then data is missing or participants has duplicate performance.
  dplyr::select(-modal_racism_scale_rows)

participants_with_full_data <-
  dplyr::semi_join(participants_with_full_data, modern_racism_scale, by = "participant")


amp_recognition_scale <-
  trimmed_df %>%
  dplyr::filter(!is.na(amp_recognition_response)) %>%
  dplyr::select(participant, amp_recognition_response)

participants_with_full_data <-
  dplyr::semi_join(participants_with_full_data, amp_recognition_scale, by = "participant")


# participants with at least partial data
participants_with_at_least_partial_data <-
  trimmed_df %>%
  dplyr::distinct(participant) %>%
  dplyr::mutate(participant_partial = participant) 

# participants with incomplete data
participants_with_incomplete_data <-
  dplyr::anti_join(participants_with_at_least_partial_data, participants_with_full_data, by = "participant")


# screened data -----------------------------------------------------------

screened_data <- 
  semi_join(trimmed_df, participants_with_full_data, by = "participant")
 
screened_data %>% write.csv("screened data.csv", row.names = FALSE)


# make lists of participants ----------------------------------------------


## produce prolific codes so that participants can be credited or rejected

# 1. prolific codes for participants with complete data so that they can be paid
prolific_codes_for_complete_participants <-
  dplyr::inner_join(trimmed_df, participants_with_full_data, by = "participant") %>%
  dplyr::filter(trialcode == "ProlificCode") %>%
  dplyr::select(participant, response) %>%
  dplyr::distinct(participant, .keep_all = TRUE) 

# participants to credit
prolific_codes_for_complete_participants %>% readr::write_csv("prolific codes - complete data.csv")

# 1.1 N of complete participants
prolific_codes_for_complete_participants %>% dplyr::summarize(participant = n())


# 2. prolific codes for participants with incomplete data so that they can be rejected
prolific_codes_for_incomplete_participants <-
  dplyr::inner_join(trimmed_df, participants_with_incomplete_data, by = "participant") %>%
  dplyr::filter(trialcode == "ProlificCode") %>%
  dplyr::select(participant, response) %>%
  dplyr::distinct(participant, .keep_all = TRUE) 

prolific_codes_for_incomplete_participants %>% readr::write_csv("prolific codes - incomplete data.csv")

# 2.1 N of incomplete participants
prolific_codes_for_incomplete_participants %>% dplyr::summarize(participant = n())

# 2.2 rejected participants will automatically have their slots reopened and fresh data will be collected.


# 3 Misfits on the prolific site
# the list of participants on prolific will still probably include a number of remaining participants. for example:
# 1. Those who cancelled all tasks (and therefore recorded no inquisit data but nonetheless returned a 
# completion code to prolific). These should be REJECTED. If they are not, they'll be paid automatically after two weeks. 
# 2. Those who entered an incorrect prolific id or did not enter one, but who nonetheless produced legitimate data.  
# These should be APPROVED.

