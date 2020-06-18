# ------------------------------------------------------------------------------
# Title: Make config file for variables to use in following models
# Author: Ryan Gan and Sarah McGough
# Purpose: Builds config files so we don't need to have too many repeated lists
# across analytics scripts
# ------------------------------------------------------------------------------

# library
library(tidyverse)
library(yaml)

# Study end date to make sure results are reproducible -------------------------

# Number of bootsraps to run

# setting end date to '2020-05-13' for manuscript

end_date <- '2020-05-13'
####
# Commented out code to use last date of google mobility as end date for paper 
# taking last date mobility is available
# end_date <- read_csv('./data/source_data/google-mobility.csv') %>% 
#   select(date) %>% 
#   summarize(end_date = max(date)) %>% 
#   pull(end_date)
####


# add end date to list

study_window <- list(
  end_date = as.character(end_date)
)

# Metropolitan counties of interest --------------------------------------------
# study fips 
study_fips <- as.character(
  c(
    "06001","06037","06059","06075","06081","06085","17031","17043","17089",
    "17097","17111","17197","18089","22051","22071","22075","22087","22103", 
    "26099","26125","26163","36005","36047","36061","36081","36085","53033",
    "53061"
    )
)

# Read in RWJ Data Dictionary --------------------------------------------------
# read in roche variable names for rwj
roche_rwj <- read_csv('./references/data_dictionary/rwj_roche_var_names.csv')

# read in rwj datakey
rwj_key <- readxl::read_excel(
  './references/data_dictionary/rwj_data_dictionary_2020.xlsx'
  ) %>% 
  rename(
    variable_name = `Variable Name`,
    measure = Measure,
    description = Description
    ) %>% 
  # make index variable for subsetting
  rowid_to_column('column_index') 

# rwj variables used in project (sorted)
# indexs of variables we plan to use in principle components analysis and 
# descriptive figures

rwj_vec <- sort(
    c(
      1:8,34,49,307,626,631,671,666,503,183,188,193,218,228,523,528,
      533,397,75,70,258,263,273,611,616,268,135,140,153,173,462, 621
      )
    )

# subset table to specific variables
rwj_vars <- rwj_key[rwj_vec, ] %>% 
  left_join(roche_rwj, by = c('column_index')) %>% 
  arrange(column_index)

# build list to save as yaml config file

rwj_list <- list(
  # rwj variable name
  rwj_var = rwj_vars$variable_name,
  # roche names
  roche_names = rwj_vars$roche_names,
  # column index location in rwj data
  column_index = as.integer(rwj_vars$column_index)
)

# YAML config file -------------------------------------------------------------
# named list 
config_list <- list(
  study_window = study_window, 
  study_fips = study_fips,
  rwj = rwj_list
  )

print(config_list)

# write yaml config
write_yaml(
  config_list,
  './scripts/config.yaml'
  )





