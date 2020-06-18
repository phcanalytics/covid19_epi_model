# ------------------------------------------------------------------------------
# Title: Script to run all sequential scripts for project
# Author: Ryan Gan and Sarah McGough
# ------------------------------------------------------------------------------

# Setup ------------------------------------------------------------------------

# Start by installing pacman package if not present
print('Using "pacman" package to manage and install additional packages for this project')

try(
  {
    # check if pacman is installed; if not, install it
    if( "pacman" %in% installed.packages()[, 'Package'] == FALSE) {
      print('Installing "pacman"')
      install.packages('pacman', repos = 'http://cran.us.r-project.org')
    } else {
      print('pacman already installed, installing additional packages')
    }
  }
)

# needs units binary package for factorextra plots in 04 script
try(
  {
    # check if pacman is installed; if not, install it
    if( "units" %in% installed.packages()[, 'Package'] == FALSE) {
      print('Installing units package')
      install.packages("units", repos = 'http://cran.us.r-project.org', type='source')
    } else {
      print('units package already installed, installing additional packages')
    }
  }
)

# Project packages -------------------------------------------------------------
# vector of character strings of packages for project

project_packages <- c(
  'tidyverse',
  'yaml', # for yaml config file
  'RcppRoll', # for rolling average 
  'factoextra', # for pca
  'corrplot', # for pretty correlation plots
  'cowplot', # for gridded plots
  'sf', # simple features for map and some spatial manipulation
  'gridExtra', # for plotting grids
  'reshape2', # for reshaping
  'ggrepel', # for pretty/readable text on ggplot for pc1 plot
  'mgcv', # for generalized additive models
  'dlnm', # for distributed lag crossbasis creation and prediction
  'mvmeta', # for 2 stage dlnm sensitivity
  'imputeTS', # for rolling average impuation of missing mobility; note missing mobility is small
  'wesanderson', # for pretty color schemes
  'parallel', # for parallel boots; should be part of base R 
  'foreach', # for parallel boots; should be part of base R 
  'doParallel' # for parallel boots; should be part of base R 
)

# load project packages; pacman will install if it's not present
print('load project packages; pacman will install if not present')
pacman::p_load(char = project_packages)

# Run Scripts ------------------------------------------------------------------
# start run time
start_time <- Sys.time()
  
print('Running script to collect data (if necessary)')
source('./scripts/01-collect_data.R')

print('Running script to build config file used in subsequent scripts')
source('./scripts/02-make_config_file.R')

print('Running script to make master time series')
source('./scripts/03-make_nyt_time_series_county.R')

print('Running script to run principle components analysis')
source('./scripts/04-county_principle_components_analysis.R')

print('Running script to build analysis dataframes')
source('./scripts/05-analysis_dataframe.R')

print(
  paste0(
    'Clearing global environment up to this point. ',
    'Only need the analysis_df now. Getting rid of large',
    ' time series data in environment to free up memory for analysis'
  )
)

# clear environment; packages should still be loaded
try(rm(list = ls()))

# Try to run figure 1 plot, but since requires sf, will put some error handling 
# for submission of the script to still run 
status <- tryCatch(
  # try to execute script
  {
  print('Attempting to run script to make figure 1 plot')
  source('./scripts/06-descriptive_figures.R')  
  },
  error = function(e) {
    print('Did not run; likely need to install sf and gdal (from source)')
    }
  )

# Before running the parallel bootstrap of the main gam, check number of cores
# If cores at least 20, will run a 10k bootstrap iteration; takes about 3 hours
# If cores available less than 20, run a smaller bootstrap sample of 100 on
# a local for loop

print('Running script for all analyses')

if (parallel::detectCores() < 20) {
  print('Running locally; will run 100 iterations and use all but 1 of system cores')
  print('Note this took 25 minutes on an 8 core Mac Book Pro')
} else {
  print('Running bootstraps in parallel; will run 10k iterations and use 20 cores')
  print('Note this took 3 hours with 20 cores')
} 

source('./scripts/07-main_gam_boot.R')

print('Compiling results based on boot estimates')
source('./scripts/08-boot_results.R')

print(
  paste0(
    'Running additional sensitivity analyses; ',
    'note using mgcv bayes posterior confidence intervals ',
    'because coverage is pretty good')
  )
source('./scripts/09-sensitivity_analyses.R')

print('Running sensitivity analysis for 2-stage dlnm models')
source('./scripts/10-dlnm_twostage.R')

print('Saving summary information')
# end time
try(
  {
  end_time <- Sys.time()
  run_time <- end_time - start_time
  }
)

# print runtime and session_info
# print out session info for reproducibility/transparency; saving in script folder
try(
  {
  sink('./results/00-overall_session_info.txt')
  print('RUN TIME:  ')
  print(run_time)
  
  print('#######################################################################')
  
  print('SESSION INFORMATION:  ')
  print(sessionInfo())
  sink()
  }
)

