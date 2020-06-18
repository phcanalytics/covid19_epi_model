# ------------------------------------------------------------------------------
# Title: Two stage distributed lag
# Author: Sarah McGough and Ryan Gan
# Purpose: Sensitivity analysis of dlnm models for each county, pooled in meta 
# analysis.
# ------------------------------------------------------------------------------

### Adapted from code provided by Gasparrini and Armstrong 
# 'Reducing and meta-analysing estimates from distributed lag non-linear models'
# Published 2013: BMC Medical Research Methodology

# setup
library(tidyverse)
library(dlnm)
# meta for multi location dlnm
library(mvmeta) # multivariate meta by gasparrini for two stage lag
library(mgcv)


analysis_df <- read_csv('./data/05-analysis_df.csv') %>% 
  # starting at 30 days prior to time since first death in order to get lagged
  # values 30 days prior
  filter(time_since_first_death >= (-30)) %>% 
  # code metro_state_county as factor
  mutate(metro_state_county = as.factor(metro_state_county))



# Two-stage meta analysis; probably going to be messy with counties ------------
fips <- unique(analysis_df$fips)

# data as list
data <- lapply(fips, function(x) 
  analysis_df[analysis_df$fips==x,])

names(data) <- fips

m <- length(fips)

# mobility ranges from data
ranges <- t(
  sapply(
    data, 
    function(x) range(x$retail_and_recreation_percent_change_from_baseline, 
                      na.rm=T)
  )
)

# Define --------------------------------------------------------
# MAIN MODEL
# - PREDICTOR SPACE: QUADRATIC SPLINE WITH SPECIFIC KNOT SELECTION
# - LAG SPACE: NATURAL CUBIC SPLINE WITH DF AT EQUALLY-SPACED LOG-VALUES
lag <- c(0,30)
bound <- colMeans(ranges)

# Define argument function for cubic
argvar <- list(fun='cr', df=4)
arglag <- list(fun='cr', df=4)

####################################################################
# BUILT OBJECTS WHERE RESULTS WILL BE STORED
#   y- IS THE MATRIX FOR THE OUTCOME PARAMETERS
#   S- IS THE LISTS OF (CO)VARIANCE MATRICES

# Mobility levels evaluated
# c("-80","-70","-60","-50", "-25","-10", "0", "5", '10')

# PREDICTOR-SPECIFIC SUMMARIES FOR MAIN MODEL
yall <- y80 <- y70 <- y60 <- y50 <- y25 <- ym10 <- y0 <- y5 <- y10 <- matrix(
  NA,length(data),4,dimnames=list(fips,paste("b",seq(4),sep=""))
  ) # matrix for all estimations of mobility

# (CO)VARIANCE MATRICES
Sall <- vector("list",length(data))
names(Sall) <- fips
S80 <- S70 <- S60 <- S50 <-S25 <- Sm10 <- S0 <- S5 <- S10 <- Sall


####################################################################
# RUN THE MODEL FOR EACH County

# WARNING FOR PREDICTION BEYOND BOUNDARIES SUPPRESSED
options(warn=-1)

# LOOP FOR STATES
for(i in seq(data)) {

  # PRINT
  cat(i,"")
  
  print(names(data[i]))
  # LOAD
  sub <- data[[i]]
  plot(sub$time_since_first_death, sub$daily_deaths)
  try({
    # DEFINE THE CROSS-BASES
    cb <- crossbasis(sub$retail_and_recreation_percent_change_from_baseline,
                     lag=lag,argvar=argvar,arglag=arglag)
    
    # penalize 
    cbPen <- cbPen(cb)
    
    # RUN THE FIRST-STAGE MODELS
    mfirst <- gam(daily_deaths ~ cb + s(time_since_first_case),
                  family=quasipoisson(),
                  paraPen=list(cb=cbPen), 
                  method='REML',
                  sub)
    
    # REDUCTION TO SUMMARY ASSOCIATIONS -----
    crall <- crossreduce(cb,mfirst,cen=0)
    cr80 <- crossreduce(cb,mfirst,type="var",value=-80,cen=0)
    cr70 <- crossreduce(cb,mfirst,type="var",value=-70,cen=0)
    cr60 <- crossreduce(cb,mfirst,type="var",value=-60,cen=0)
    cr50 <- crossreduce(cb,mfirst,type="var",value=-50,cen=0)
    cr25 <- crossreduce(cb,mfirst,type="var",value=-25,cen=0)
    crm10 <- crossreduce(cb,mfirst,type="var",value=-10,cen=0)
    cr0 <- crossreduce(cb,mfirst,type="var",value=0,cen=0)
    cr5 <- crossreduce(cb,mfirst,type="var",value=5,cen=0)
    cr10 <- crossreduce(cb,mfirst,type="var",value=10,cen=0)
    
    # STORE THE RESULTS ----
    
    # Cumulative summary
    yall[i,] <- coef(crall)
    Sall[[i]] <- vcov(crall)
    
    # PREDICTOR-SPECIFIC SUMMARY FOR MOBILITY SLICES (MAIN MODEL)
    y80[i,] <- coef(cr80)
    S80[[i]] <- vcov(cr80)
    
    y70[i,] <- coef(cr70)
    S70[[i]] <- vcov(cr70)
    
    y60[i,] <- coef(cr60)
    S60[[i]] <- vcov(cr60)
    
    y50[i,] <- coef(cr50)
    S50[[i]] <- vcov(cr50)
    
    y25[i,] <- coef(cr25)
    S25[[i]] <- vcov(cr25)
    
    ym10[i,] <- coef(crm10)
    Sm10[[i]] <- vcov(crm10)
    
    y0[i,] <- coef(cr0)
    S0[[i]] <- vcov(cr0)

    y5[i,] <- coef(cr5)
    S5[[i]] <- vcov(cr5)
    
    y10[i,] <- coef(cr10)
    S10[[i]] <- vcov(cr10)
  })
}


# SELECT THE ESTIMATION METHOD
method <- "reml"

# META All; not used in this analysis as we are not interested in the cumulative 
# effects at this point
mvall <- mvmeta(na.omit(yall)~1, na.omit(Sall), method=method)
summary(mvall)

# PREDICTOR-SPECIFIC SUMMARY FOR MOBILITY (MAIN MODEL)
mv80 <- mvmeta(y80~1,S80,method=method)
summary(mv80)

mv70 <- mvmeta(y70~1,S70,method=method)
summary(mv70)

mv60<- mvmeta(y60~1,S60,method=method)
summary(mv60)

mv50 <- mvmeta(y50~1,S50,method=method)
summary(mv50)

mv25 <- mvmeta(y25~1,S25,method=method)
summary(mv25)

mvm10 <- mvmeta(ym10~1,Sm10,method=method)

# summary won't work here
mv0 <- mvmeta(y0~1,S0,method=method)

mv5 <- mvmeta(y5~1,S5,method=method)
summary(mv5)

mv10 <- mvmeta(y10~1,S10,method=method)
summary(mv10)

# CREATE BASES FOR PREDICTION -----
# using bound object 1 and 2 by 1 mobility

xvar <- seq(bound[1],bound[2],by=1)
bvar <- do.call("onebasis",c(list(x=xvar),attr(cb,"argvar")))
# lag over 30 days 
xlag <- 0:300/10
blag <- do.call("onebasis",c(list(x=xlag),attr(cb,"arglag")))

# County-SPECIFIC FIRST-STAGE SUMMARIES -----
regall <- lapply(seq(nrow(yall)),
                 function(i) crosspred(
                   bvar,coef=yall[i,], vcov=Sall[[i]],model.link="log",cen=0))

reg80 <- lapply(seq(nrow(y80)),
                function(i) crosspred(
                  blag,coef=y80[i,], vcov=S80[[i]], model.link="log",cen=0))

reg70 <- lapply(seq(nrow(y70)),
                function(i) crosspred(
                  blag,coef=y70[i,], vcov=S70[[i]], model.link="log",cen=0))

reg60 <- lapply(seq(nrow(y60)),
                function(i) crosspred(
                  blag,coef=y60[i,], vcov=S60[[i]], model.link="log",cen=0))

reg50 <- lapply(seq(nrow(y50)),
                function(i) crosspred(
                  blag,coef=y50[i,], vcov=S50[[i]], model.link="log",cen=0))

reg25 <- lapply(seq(nrow(y25)),
                function(i) crosspred(
                  blag,coef=y25[i,], vcov=S25[[i]], model.link="log",cen=0))

regm10 <- lapply(seq(nrow(ym10)),
                function(i) crosspred(
                  blag,coef=ym10[i,], vcov=Sm10[[i]], model.link="log",cen=0))

reg0 <- lapply(seq(nrow(y0)),
               function(i) 
                 crosspred(blag,coef=y0[i,], vcov=S0[[i]],model.link="log",cen=0))

reg5 <- lapply(seq(nrow(y5)),
               function(i) 
                 crosspred(blag,coef=y5[i,], vcov=S5[[i]],model.link="log",cen=0))

reg10 <- lapply(seq(nrow(y10)),
               function(i) 
                 crosspred(blag,coef=y10[i,], vcov=S10[[i]],model.link="log",cen=0))

# PREDICTION FOR A GRID OF MOBILITY AND LAG VALUES

# PREDICTOR-SPECIFIC SUMMARIES FOR Mobility (MAIN MODEL)
cp80 <- crosspred(blag,coef=coef(mv80),vcov=vcov(mv80),
                  model.link="log",at=0:300/10,cen=0)

cp70 <- crosspred(blag,coef=coef(mv70),vcov=vcov(mv70),
                  model.link="log",at=0:300/10,cen=0)

cp60 <- crosspred(blag,coef=coef(mv60),vcov=vcov(mv60),
                  model.link="log",at=0:300/10,cen=0)

cp50 <- crosspred(blag,coef=coef(mv50),vcov=vcov(mv50),
                  model.link="log",at=0:300/10,cen=0)

cp25 <- crosspred(blag,coef=coef(mv25),vcov=vcov(mv25),
                  model.link="log",at=0:300/10,cen=0)

cpm10 <- crosspred(blag,coef=coef(mvm10),vcov=vcov(mvm10),
                  model.link="log",at=0:300/10,cen=0)

cp0 <- crosspred(blag,coef=coef(mv0),vcov=vcov(mv0),
                 model.link="log",at=0:300/10,cen=0)

cp5 <- crosspred(blag,coef=coef(mv5),vcov=vcov(mv5),
                 model.link="log",at=0:300/10,cen=0)

cp10 <- crosspred(blag,coef=coef(mv10),vcov=vcov(mv10),
                 model.link="log",at=0:300/10,cen=0)


# Plot of DLNM pooled results
crosspred_lists <- list(cp80, cp70, cp60, cp50, cp25, cpm10, cp0, cp5, cp10)
mobility <- c(-80, -70, -60, -50, -25, -10, 0, 5, 10)

# bind lists to a row
pooled_est <-  map2_dfr(
  .x = crosspred_lists,
  .y = mobility, 
  ~ data.frame(
      mobility = .y, 
      lag = .x$predvar, 
      rr = .x$matRRfit, 
      lower_95 = .x$matRRlow, 
      upper_95 = .x$matRRhigh
    ) %>% 
    rename(rr = lag0, lower_95 = lag0.1, upper_95 = lag0.2)
  )


gam_lag_plot <- ggplot() +
  geom_line(data = pooled_est, aes(x=lag, y=rr), color = 'darkblue') + 
  geom_ribbon(
    data = pooled_est, aes(x = lag, ymin=lower_95, ymax=upper_95), alpha = 0.5, fill = 'lightblue') +
  facet_wrap(~mobility) +
  geom_hline(data = data.frame(ref = 1), aes(yintercept = 1), color = 'red', linetype = 'dashed') +
  ylab('Relative Daily COVID19 Death Rate') +
  xlab('Days After') +
  theme_minimal() +
  theme(legend.position = 'bottom')


# NOTE WILL PROBABLY DELETE THE SECOND VERSION OF BLOCK BOOT SO THIS WILL BECOME SCRIPT 10
# save gam plot
ggsave(
  filename = './results/10-dlnm_twostage_gam_lag_mobility.pdf',
  plot = gam_lag_plot,
  height = 8,
  width = 8,
  unit = 'in'
)

print('Done with 2-stage dlnm')
