# ------------------------------------------------------------------------------
# Title: Bootstrapped uncertainty estimates for main GAM
# Author: Sarah McGough
# Purpose: Variance estimates from GAM objects with time series data can be
# artificially low, leading to tighter uncertainty estimates. Here we bootstrap
# estimates of uncertainty of the main GAM. This is run in parallel on 20 cores 
# and takes about 20 minutes. If run locally, this could take a multiple hours.
# ------------------------------------------------------------------------------

# Parametric GAM bootstrap- not residuals

# libraries 
library(tidyverse)
library(mgcv) # for gam 
library(dlnm)
library(foreach)
library(doParallel)
library(parallel)

# Read yaml config file --------------------------------------------------------
config <- read_yaml('./scripts/config.yaml')

# Read analysis dataframe
analysis_df <- read_csv('./data/05-analysis_df.csv') %>% 
  # starting at config$max_lag (60) days prior to time since first death in order to get lagged
  # values config$max_lag (60) days prior 
  filter(time_since_first_death >= (-(config$max_lag))) %>% 
  # code metro_state_county as factor
  mutate(metro_state_county = as.factor(metro_state_county))


# build crossbasis for gam model for 60 day lagged mobility of retail and rec
# called cbgam 
retail_basis <- crossbasis(
  analysis_df$retail_and_recreation_percent_change_from_baseline, 
  lag = config$max_lag,
  argvar = list(fun='cr', df=4), # using penalized spline 'cr'
  arglag = list(fun='cr', df=4),
  group = analysis_df$metro_state_county # makes sure I lag appropriately by metro/county
)


# penalize the crossbasis splines
retail_basisPen <- cbPen(retail_basis) 

# ADD NEW MOBILITY CATEGORIES: build cross-basis for each

workplace_basis <- crossbasis(
  analysis_df$workplaces_percent_change_from_baseline, 
  lag = config$max_lag,
  argvar = list(fun='cr', df=4), # using penalized spline 'cr'
  arglag = list(fun='cr', df=4),
  group = analysis_df$metro_state_county # makes sure I lag appropriately by metro/county
)


# penalize the crossbasis splines
workplacePen <- cbPen(workplace_basis) 

# Another category
parks_basis <- crossbasis(
  analysis_df$parks_percent_change_from_baseline, 
  lag = config$max_lag,
  argvar = list(fun='cr', df=4), # using penalized spline 'cr'
  arglag = list(fun='cr', df=4),
  group = analysis_df$metro_state_county # makes sure I lag appropriately by metro/county
)

# penalize the crossbasis splines
parksPen <- cbPen(parks_basis) 


## Ryan testing stuff ----

# dimension of parks basis not missing data
rbasis_nomiss <- retail_basis[complete.cases(retail_basis),]

plot(rbasis_nomiss[1,])
# 16 columns
dim(rbasis_nomiss)
# i think this is the first set of values the basis transforms; reversing so it's in order
retail_example <- pred_df$retail_and_recreation_percent_change_from_baseline[1:60]


dim <- dim(as.matrix(analysis_df$retail_and_recreation_percent_change_from_baseline))
check <- onebasis(analysis_df$retail_and_recreation_percent_change_from_baseline, fun = 'cr', df = 4)

attributes(check)$S
var_basis
summary(analysis_df$retail_and_recreation_percent_change_from_baseline)

# get out basis for lag and variable
attributes(retail_basis)
# same obs
plot(round(retail_basis[61,],1))
plot(rev(retail_example))
rbasis_nomiss[1,]

?crossbasis

analysis_df[61,]$retail_and_recreation_percent_change_from_baseline
basislag <- attributes(retail_basis)$arglag$S
basisvar <- attributes(retail_basis)$argvar$S

basislag[, 1] 


crossbasis <- matrix(0, nrow = dim[1], ncol = ncol(basisvar) * 
                       ncol(basislag))

seqlag <-function(lag,by=1) seq(from=lag[1],to=lag[2],by=by)
basisvar[,1]
lag1 <- mklag(60)
group = NULL


rev(retail_example)
basislag

basisvar
?tsModel::Lag
tsModel::Lag(basisvar[,1], seqlag(lag1))
as.matrix(tsModel::Lag(basisvar[,1]), seqlag(lag1))

# extract existing lag basis matrix
basislag <- attributes(retail_basis)$arglag$S
basislag
# extract existing variable basis matrix
basisvar <- attributes(retail_basis)$argvar$S
# kronecker product
(basisvar %x% basislag)

### Note suggest figuring out the function below and 
# applying the existing basis matrices to counterfactual labs values


diff(lag) + 1
tsModel::Lag(basisvar[,1],)
seqlag(lag1)
analysis_df$retail_and_recreation_percent_change_from_baseline %>% 
attributes(retail_basis)$dim

attributes(retail_basis)$lag
tsModel::Lag(basisvar[, v], seqlag(lag1), 
                         group = group)


tsModel::Lag(analysis_df$retail_and_recreation_percent_change_from_baseline, 60, 
             group = analysis_df$metro_state_county)

## one basis
onebasis(analysis_df$retail_and_recreation_percent_change_from_baseline, fun = 'cr', df = 4)



# functions
for (v in seq(length = ncol(basisvar))) {
  
  if (dim[2] == 1L) {
    mat <- as.matrix(tsModel::Lag(basisvar[, v], seqlag(lag1), 
                         group = group))
  }
  else mat <- matrix(basisvar[, v], ncol = diff(lag) + 
                       1)
  for (l in seq(length = ncol(basislag))) {

    crossbasis[, ncol(basislag) * (v - 1) + l] <- mat %*% 
      (basislag[, l])
  }
}

mat
seqlag(lag, by = 1)

lag <- 60

seqlag

seq(from = 0, to = 60)
for (v in seq(length = ncol(basisvar))) {
  print(v)
}
seqlag(mklag(60))

mklag <- 
  function(lag) {
    #
    ################################################################################
    #
    #  lag MUST BE A POSITIVE INTEGER VECTOR 
    if(any(!is.numeric(lag))||length(lag)>2) 
      stop("'lag' must a integer vector or length 2 or 1")
    if(length(lag)==1L) lag <- if(lag<0L) c(lag,0L) else c(0L,lag)
    if(diff(lag)<0L) stop("lag[1] must be <= lag[2]")
    return(round(lag[1L:2L]))
  }

mklag(60)

retail_basis
mat <- as.matrix(tsModel::Lag(basisvar[, 1], seqlag(60)))

### RYAN NOTE: crossbasis function calls onebasis, which itself produces a matrix
# note this doesn't seem to recognize groups like crossbasis does?
var_basis <- onebasis(analysis_df$retail_and_recreation_percent_change_from_baseline,
         fun = 'cr', df = 4)
# i think it's working off the 11239 by 4 matrix here:
dim(var_basis)
# but I think  we can use the existing matrix from one basis to apply the transformation
# to the counterfactual observatiosns as they are the same as the ones produced in the
# crossbasis; see below
all.equal(attributes(var_basis)$S, # onebasis matrix
          basisvar) # crossbasis matrix

# we should be able to apply these individual basis matrices already learned from the observed
# data to counterfactual data. 



## End of Ryan trying to figure out stuff -----


# 1. Fit GAM --------------------------------------------------------------

dlnm.main.gam_multimob3 <- gam(
  daily_deaths ~
    retail_basis + # this is the lag matrix 
    #workplace_basis +
    parks_basis + 
    s(time_since_first_death) + 
    s(time_since_first_death, metro_state_county, bs=c("fs")) + # global smoother
    s(PC1, bs="cr", k=5) +
    s(PC2, bs="cr", k=5) +
    s(PC3, bs="cr", k=5) +
    s(PC4, bs="cr", k=5) +
    offset(log(population_v051)), 
  data=analysis_df,
  paraPen=list(retail_basis=retail_basisPen,
              # workplace_basis=workplacePen,
               parks_basis=parksPen),# this applies the penalty to the lagged matrix
  family="quasipoisson",
  method="REML"
)

summary(dlnm.main.gam_multimob3)

test <- predict(dlnm.main.gam_multimob3, type = 'response')

summary(test)

glimpse(analysis_df)

pred_df <- analysis_df %>% 
  filter(time_since_first_death >= 0) %>% 
  filter(!is.na(daily_deaths) & !is.na(parks_percent_change_from_baseline) &
         !is.na(retail_and_recreation_percent_change_from_baseline))


pred_df$pred_deaths <- predict(dlnm.main.gam_multimob3, type = 'response')


ggplot() +
  geom_line(
    data = pred_df, 
    aes(x=time_since_first_death, y = (daily_deaths/population_v051) * 100000, group = fips),
    color = 'black') +
  geom_line(
    data = pred_df, 
    aes(x=time_since_first_death, y = (pred_deaths/population_v051) * 100000, group = fips),
    color = 'blue'
  ) +
  facet_wrap(~metro_area)

# Before saving final, add same results from main GAM to list for plots
original <- plot(dlnm.main.gam, pages=1, residuals=TRUE, seWithMean = TRUE)

# Check autocorrelation
pacf(residuals(dlnm.main.gam, type="scaled.pearson"))
pacf(residuals(dlnm.main.gam)) # basically the same

# png partial autocorrelation (PACF) plots
png("./results/07-pacf_mainmod.png",width=10, height=7, units="in", res=300)
pacf(residuals(dlnm.main.gam, type="scaled.pearson"))
dev.off()

# County-specific principle components 1 estimates
# get state abbreviations
states <- data.frame(datasets::state.name, datasets::state.abb) 
# rename column
colnames(states) <- c('state.name', 'state.abb')

# predicted value for each county based on pc1
county_pc1_pred <- analysis_df %>% 
  filter(time_since_first_death >= 0) %>% 
  bind_cols(predicted=original[[3]]$p.resid) %>% 
  group_by(metro_state_county, metro_area, county, state) %>% 
  summarise(PC1=mean(PC1),predicted=mean(predicted)) %>%
  left_join(states, by=c("state"="state.name")) %>%
  mutate(county_state=paste(county,state.abb,sep=", "))

# predicted value for each county based on pc2
county_pc2_pred <- analysis_df %>% 
  filter(time_since_first_death >= 0) %>% 
  bind_cols(predicted=original[[4]]$p.resid) %>% 
  group_by(metro_state_county, metro_area, county, state) %>% 
  summarise(PC2=mean(PC2),predicted=mean(predicted)) %>%
  left_join(states, by=c("state"="state.name")) %>%
  mutate(county_state=paste(county,state.abb,sep=", "))

# predicted value for each county based on pc3
county_pc3_pred <- analysis_df %>% 
  filter(time_since_first_death >= 0) %>% 
  bind_cols(predicted=original[[5]]$p.resid) %>% 
  group_by(metro_state_county, metro_area, county, state) %>% 
  summarise(PC3=mean(PC3),predicted=mean(predicted)) %>%
  left_join(states, by=c("state"="state.name")) %>%
  mutate(county_state=paste(county,state.abb,sep=", "))

# predicted value for each county based on pc4
county_pc4_pred <- analysis_df %>% 
  filter(time_since_first_death >= 0) %>% 
  bind_cols(predicted=original[[6]]$p.resid) %>% 
  group_by(metro_state_county, metro_area, county, state) %>% 
  summarise(PC4=mean(PC4),predicted=mean(predicted)) %>%
  left_join(states, by=c("state"="state.name")) %>%
  mutate(county_state=paste(county,state.abb,sep=", "))

# DLNM output for main model
dlnm_pred_retail <- crosspred(
  retail_basis,
  dlnm.main.gam_multimob3,
  at=10:-80,
  by=1,
  bylag=1,
  ci.level = 0.95,
  cen=config$ref_lag
)

# DLNM output for second mobility category
dlnm_pred_workplace <- crosspred(
  workplace_basis,
  dlnm.main.gam_multimob2,
  at=10:-80,
  by=1,
  bylag=1,
  ci.level = 0.95,
  cen=config$ref_lag
)

# DLNM output for third mobility category
dlnm_pred_parks <- crosspred(
  parks_basis,
  dlnm.main.gam_multimob3,
  at=10:-80,
  by=1,
  bylag=1,
  ci.level = 0.95,
  cen=config$ref_lag
)


# saving only dlnm fit to log link across prediction matrix
dlnm_data_main <- data.frame(dlnm_pred_main$matfit) %>%
  rownames_to_column('mobility') %>%
  gather('lag_day', 'fit', -mobility) %>%
  mutate(
    rr = exp(fit),
    lag_day = gsub('lag', '', lag_day)
  ) %>%
  mutate_all(as.numeric)

# Extract x/y values for partial effects plots, plus residuals
main_model_list <- list(
  # Time trend
  temporal=list(
    plotme=data.frame(x=original[[1]]$x, ystar=original[[1]]$fit)
  ),
  # PC1
  pc1=list(
    plotme=data.frame(x=original[[3]]$x, ystar=original[[3]]$fit)
  ),
  # PC2
  pc2=list(
    plotme=data.frame(x=original[[4]]$x, ystar=original[[4]]$fit)
  ),
  # PC3
  pc3=list(
    plotme=data.frame(x=original[[5]]$x, ystar=original[[5]]$fit)
  ),
  # PC4
  pc4=list(
    plotme=data.frame(x=original[[6]]$x, ystar=original[[6]]$fit)
  ),
  # County PC1 predictions
  county_pc1 = county_pc1_pred, 
  # County PC2 predictions
  county_pc2 = county_pc2_pred,
  # County PC3 predictions
  county_pc3 = county_pc3_pred,
  # County PC4 predictions
  county_pc4 = county_pc4_pred,
  # DLNM mobility
  dlnm = dlnm_data_main,
  # Coefficients
  coefs= coef(dlnm.main.gam),
  # VCOV
  vcov_coefs = vcov(dlnm.main.gam)
)

# save model reults list to call later for bootstrap calculations of conf int.
save(main_model_list, file='./data/07-main_model_list.RData')

# Parallelization of bootstrap -------------------------------------------------
# PARALLELIZED; If not more than 20 cores, run with as many possible; else run with 20
if (detectCores() < 20) {
  print('Running locally; will run 100 iterations and use as many cores (-1) as possible')
  print('Note: R versions > 4.0 on MacOS seem to have a bug in parallel package')
  print('To fix this, we specified setup_strategy = sequential; unclear if this works for pcs')
  print('This took 25 minutes on a 8 core macbook pro')
  
  # set up local cluster; a normal for loop would also work with some modifications
  # if the user didn't want to run using foreach package
  cl <- makeCluster(
    detectCores() - 1, # use all but one cores
    setup_strategy = "sequential" # for R versions > 4.0 on MacOS, this should work; parallel has a bug
    )
  
  # set n for number of iterations
  n <- 100

} else {
  print('Running bootstraps in parallel; will run 10k iterations and use 20 cores')
  print('If this times out on a computing cluster, consider detaching screen or tmux session')
  cl <- makeCluster(20, setup_strategy = "sequential")
  n <- 1000 # setting to 1000 to test
  # n <- 10000
  
  ### NOTE ON BOOTSTRAP ###
  # THIS CAN TAKE A LONG TIME TO RUN. 1000 ITERATIONS IS LIKELY SUFFICIENT
  # THIS TAKES APPROXIMATELY 20 MINUTES ON 20 CORES
  # n <- 1000 # change to n = 1000 iterations if you want to speed up computation
} 

# register the cluster
clusterSetRNGStream(cl, 9956)
registerDoParallel(cl)

# 2. Obtain fitted values -------------------------------------------------

yhat <- dlnm.main.gam$fitted.values

# 3. Make simulated datasets using quasi-Poisson draws --------------------

# Quasi-poisson function
rqpois <- function(n, lambda, phi) {
  mu = lambda
  r = rnbinom(n, size=mu/phi/(1-1/phi),prob=1/phi)
  return(r)
}

# These two functions are equivalent. To prove it, try rqpois(100, 4, 4.5) for each after set.seed(8)
# rqpois <- function(n, mu, theta) {
#   rnbinom(n = n, mu = mu, size = mu/(theta-1))
# }

# gam$scale is the overdispersion parameter or phi
# which means this is an appropriate entry for "phi" in the rqpois function above


# Simulating quasipoisson noise around expected estimate from fitted gam

# get scale for quasipoisson based on gam fit
scale.param <- dlnm.main.gam$scale 

r <- length(dlnm.main.gam$fitted.values) # n is set based on number of cores
# sim data
sim_data <- matrix(nrow=r, ncol=n) 

set.seed(7)
for(i in 1:n){
  # Draw from quasi-poisson, with scale as offset
  sim_data[,i] <- rqpois(r, dlnm.main.gam$fitted.values, scale.param)
}

print('Sim of new outcomes done')

# 4. Run GAM on each of the simulated datasets (parallelized) -----------------------

# Extract original data from analysis_df
y = analysis_df %>% 
  filter(time_since_first_death>=0) %>% 
  select(time_since_first_death,PC1, PC2,PC3,PC4, population_v051, daily_deaths, fips) %>% 
  filter(complete.cases(.)) %>% 
  select(fips, y=daily_deaths, time_since_first_death, PC1, PC2,PC3,PC4, population_v051)

# cbgam needs to have na.omit()
cbgam_nomiss <- na.omit(cbgam)

print('Starting bootstrap')
# start time of for loop
start_time <- Sys.time() # time that!

# parallel for loop
boot_results_list <- foreach(
  i=1:n, 
  .export = c("sim_data", "y", "cbgam_nomiss", "cbgamPen", "cbgam"),
  .packages = c("mgcv","tidyverse","dlnm")
  ) %dopar% {
  # start time for i
  ti_start <- Sys.time()
    
  print(i)
  # Make bootstrap df
  bootstrap_df <- data.frame(
    ystar=sim_data[,i],
    y
  )
  
  # GAM on bootstrap df
  bootstrap.gam <- gam(
    ystar ~
      cbgam_nomiss + # this is the lag matrix
      s(time_since_first_death) +
      s(time_since_first_death, fips, bs=c("fs")) + # global smoother
      s(PC1, bs="cr", k=5) +
      s(PC2, bs="cr", k=5) +
      s(PC3, bs="cr", k=5) +
      s(PC4, bs="cr", k=5) +
      offset(log(population_v051)),
    data=bootstrap_df %>%
      mutate(fips=factor(fips)),
    paraPen=list(cbgam_nomiss=cbgamPen),# this applies the penalty to the lagged matrix
    family="quasipoisson",
    method="REML"
  )
  
  print('Boot GAM fit done')
  
  # # Save Output Here # #
  step1 <- plot(bootstrap.gam, pages=1, residuals=TRUE)

  # DLNM output #
  dlnm_pred <- crosspred(
    cbgam,
    bootstrap.gam,
    at=10:-80,
    by=1,
    bylag=1,
    ci.level = 0.95,
    cen=config$ref_lag,
  )
  
  # saving only dlnm fit to log link across prediction matrix
  dlnm_data <- data.frame(dlnm_pred$matfit) %>%
    rownames_to_column('mobility') %>%
    gather('lag_day', 'fit', -mobility) %>%
    mutate(
      rr = exp(fit),
      lag_day = gsub('lag', '', lag_day),
      iteration = i
    ) %>%
    mutate_all(as.numeric)
  
  # Extract x/y values for partial effects plots, plus residuals
  boot_list <- list(
    # Time trend
    temporal=list(
      plotme=data.frame(x=step1[[1]]$x, ystar=step1[[1]]$fit, iteration=i),
      resid=data.frame(fips=bootstrap_df$fips,resid=step1[[1]]$p.resid, iteration=i)
    ),
    # PC1
    pc1=list(
      plotme=data.frame(x=step1[[3]]$x, ystar=step1[[3]]$fit, iteration=i),
      resid=data.frame(fips=bootstrap_df$fips,resid=step1[[3]]$p.resid, iteration=i)
    ),
    # PC2
    pc2=list(
      plotme=data.frame(x=step1[[4]]$x, ystar=step1[[4]]$fit, iteration=i),
      resid=data.frame(fips=bootstrap_df$fips,resid=step1[[4]]$p.resid, iteration=i)
    ),
    # PC3
    pc3=list(
      plotme=data.frame(x=step1[[5]]$x, ystar=step1[[5]]$fit, iteration=i),
      resid=data.frame(fips=bootstrap_df$fips,resid=step1[[5]]$p.resid, iteration=i)
    ),
    # PC4
    pc4=list(
      plotme=data.frame(x=step1[[6]]$x, ystar=step1[[6]]$fit, iteration=i),
      resid=data.frame(fips=bootstrap_df$fips,resid=step1[[6]]$p.resid, iteration=i)
    ),
    # DLNM mobility
    dlnm = dlnm_data,
    # Coefficients
    coefs= coef(bootstrap.gam),
    # VCOV
    vcov_coefs = vcov(bootstrap.gam)
  )
  
  # parallel loop
  return(boot_list)
  # normal for loop
  # results_list[[i]] <- boot_list
  print(paste0('Done with i:', i))
  print(Sys.time() - ti_start)
}

sink(file = './results/07-boot_run_time.txt')
print(paste0('Bootstrap run time for n interations: ', i))
print(Sys.time() - start_time)
sink()

# Check file size; I don't want to overwrite the 10k iteration 
# if file size is over 1gb, it's the 10k boot and i don't want to overwrite
# wrap in try just in case it doesn't exist
try({
  # get file size if it's present
  file_size <- file.size('./data/07-boot_results_list.RData')/1024^3 # convert to gb

  # if file size is less than 2gb; probably 100 i boot, can overwrite
  # also write if file size is missing (no file present)
  if( file_size < 2 | is.na(file_size) ) {
    print('Overwriting boot data')
    # Saving list object in data
    save(boot_results_list, file='./data/07-boot_results_list.RData')
  } else {
    print('10k boot list already exists; leaving alone')
  }
  
}) # end try to write boot

# save session info up to this point since environment may be different for 
# boostrap 
sink('./results/07-bootstrap_session_info.txt')
print('SESSION INFORMATION:  ')
print(sessionInfo())
sink()


# CLOSE CLUSTER
stopCluster(cl)


