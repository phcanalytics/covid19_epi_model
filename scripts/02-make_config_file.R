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

# setting end date to '2020-12-06' for updates

end_date <- '2020-12-31'
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

# add maximum lag
max_lag <- 60

# add reference lag for mobility
ref_lag <- -20

# Metropolitan counties of interest --------------------------------------------
# study fips 
study_fips <- as.character(
  c(
    # california
    "06001","06075","06081","06085", # bay area
    "06037","06059", # los angeles and orange county
    # chicago
    "17031","17043","17089","17097","17111","17197","18089", # chicago
    # commenting out st bernards and plaquemines due to missing mobility data
    "22051","22071","22103", # new orleans; "22075", "22087" # plaquemines & st bernards
    "26099","26125","26163", # detroit
    "36005","36047","36061","36081","36085", # new york city
    "53033","53061", # seattle wa
    # texas
    "48201", "48167", # houston tx; harris and galveston county
    # "48453", # austin tx; travis
    "48113", "48439", # dallas and tarrant county tx
    # "48029", # san antonio
    "48141", # el paso
    # arizona
    "04013", # phoenix
    # minneapolis, minnesota
    "27053", # hennepin
    "27123", # ramsey 
    "27037", # dakota
    # salt lake city, utah
    "49035", # slc county
    "49049", # provo
    "49011", # davis
    # nashville, tennessee
    "47037", # davidson
    "47051", # rutherford
    # florida
    "12011", # ft. lauderdale broward 
    "12086", # miami-dade
    "12057", "12103", # tampa and st. petes
    "12105", "12095", # orlandoish: polk, orange 
    "12099" # palm beach
    #"12031" # jacksonville
  )
)
length(study_fips)
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
  rwj = rwj_list,
  max_lag = max_lag,
  ref_lag = ref_lag
  )

print(config_list)

# write yaml config
write_yaml(
  config_list,
  './scripts/config.yaml'
  )





