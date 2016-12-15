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
files <- list.files(pattern = "demographics")  

# Read these files sequentially into a single data frame
input_df <- dplyr::tbl_df(plyr::rbind.fill(lapply(files, data.table::fread, header = TRUE)))  # tbl_df() requires dplyr, rbind.fill() requires plyr, fread requires data.table


# remove prolific rows and save -------------------------------------------


# NB original file must be deleted too.

prolific_ids_trimmed <- #start with demographics
  input_df %>%
  dplyr::filter(trialcode != "ProlificCode") %>%
  write.csv("demographics.csv")

