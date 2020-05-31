#################################################################################
# This code is written by Dazhi Yang
# Singapore Institute of Manufacturing Technology (SIMTech)
# Agency for Science, Technology and Research (A*STAR)
# emails: yangdazhi.nus@gmail.com
#################################################################################

#Clear all workspace
rm(list = ls(all = TRUE))
libs <- c("dplyr", "RANN", "quantreg")
invisible(lapply(libs, library, character.only = TRUE))

#################################################################################
# Inputs
# the following setup generates forecasts at all stations for 1 resolution and 1 day
#################################################################################
# working directory
dir0 <- "/Users/DYang/Dropbox/Working papers/lasso"
# source the functions
source(file.path(dir0, "Code/functions.R"))
# change this to other date, 0801-0805, 0821, 0829, 0905-0907, 0921, 1027
day <- "20100731"
# change this to 4, 10, 30, or 60 for other aggregation
agg <- 4
# set of 21 quantiles
tau <- c(0.025, seq(0.05, 0.95, by = 0.05), 0.975)
Q <- length(tau) # 21
# zenith angle limit, data points beyond which are removed
zen.limit <- 80 

# other settings
ns <- 17 # 17 stations
nt <- ifelse(agg==60, 5, ifelse(agg==30, 10, ifelse(agg==10, 30, 75))) # number of lags
n <-  150 # query length
m <- 21 # how many analogs to select
#################################################################################

# read data
setwd(file.path(dir0, "Data", paste0("Avg-", agg, "s")))
load(paste0(day, ".RData"))

data <- data %>% filter(zen <=zen.limit) # zenith angle filter
McClear <- data$McClear # clear-sky irradiance
Tm <- data$Time # timestamps
zen <- data$zen # zenith angle
Z <- data %>% # clear-sky index matrix
  select(-one_of("Time", "zen", "McClear")) %>%
  mutate_all(., function(x) x/McClear) 
Z <- data.matrix(Z)

########################################################
# Generate forecasts for AnEn, Lag1+lpQR, and AnEn+lpQR
########################################################
AnEn = lag1lpQR = AnEnlpQR <- list()
ptm <- proc.time() # timer starts
# progress bar
pb <- txtProgressBar(max = nrow(Z), style = 3)
for(t in 1:nrow(Z)) # t is the forecast timestamp, hence, you can at most use measurements from t-1
{
  if(t < n + nt + 1) # first n + nt points are not forecast, due to insufficient training data
    next
  
  # length-n query, up to the latest measurement, i.e., the one made at t-1
  # also note that the query appears to be a matrix, but in fact consists of 17 query vectors, one for each station
  query <- t(Z[(t-n):(t-1),]) # Eq. (1), for all ns stations
  # construct database with all predictors, i.e., a total of ns x nt predictors
  database <- NULL
  for(j in 1:nt)
  {
    database <- cbind(database, Z[(t-n-j):(t-1-j),]) # Eq. (2), same for every station
  }
  # analog search using exact nearest neighbors, see 10.1063/1.5124711
  analogs <- nn2(t(database), query = query, k = m, treetype = "kd", searchtype = "standard")
  analogs <- analogs$nn.idx # this routine searches analogs for all ns stations with one pass
  
  # empty matrices to hold the quantile forecasts for one timestamp
  AnEnlpQR.tmp = AnEn.tmp = lag1lpQR.tmp <- matrix(NA, nrow = Q, ncol = ns)
  for(k in 1:ns)
  {
    # get the station and lag numbers of the selected predictors
    station <- analogs[k,] %% ns
    station <- ifelse(station==0, ns, station)
    lag <- ceiling(analogs[k,]/ns)
    
    # lasso-penalized QR with preselection (AnEn+lpQR)
    X = x0 <- NULL
    for(l in 1:m) # for each of the m analogs
    {
      X <- cbind(X, Z[(t-n-lag[l]):(t-1-lag[l]), station[l]])
      x0 <- cbind(x0, Z[t-lag[l], station[l]])
    }
    colnames(X) = colnames(x0) <- paste0("p", 1:m)
    AnEnlpQR.tmp[,k] <- QRlasso(X, x0, as.matrix(query[k,]), tau)
    
    # analog ensemble
    AnEn.tmp[,k] <- sort(x0) # sort the AnEn quantiles, lowest to highest
    
    # lasso-penalized QR with lag-1 variables (Lag1+lpQR)
    X <- database[,1:ns]
    x0 <- t(as.matrix(Z[t-1,])) # most recent 17 values
    lag1lpQR.tmp[,k] <- QRlasso(X, x0, as.matrix(query[k,]), tau)
  } # end for k
  
  lag1lpQR[[t]] <- lag1lpQR.tmp
  AnEnlpQR[[t]] <- AnEnlpQR.tmp
  AnEn[[t]] <- AnEn.tmp
  
  setTxtProgressBar(pb, t)
}
close(pb)

time.taken <- proc.time() - ptm # timer ends
time.taken/(nrow(Z)-n + nt)/ns # time taken for 1 forecast timestamp for 1 station, bear in mind there is also some time used by fitting Lag1+lpQR, so the actual time used by AnEn+lpQR is shorter.

########################################################
# Arrange forecasts by station
########################################################
# timestamps with forecasts
verification <- which(!sapply(AnEnlpQR, is.null))
# initialize an empty list to hold the forecasts at all stations
CLIM = TMPPEEN = SPTPEEN = ANEN = LAG1LPQR = ANENLPQR <- list()
for(k in 1:ns)
{
  # initialize an empty matrix to hold the forecasts from 1 station
  CLIM1 = TMPPEEN1 = SPTPEEN1 =ANEN1 = LAG1LPQR1 = ANENLPQR1 <- matrix(NA, nrow = nrow(Z), ncol = Q)
  
  ########################################################
  # AnEn, Lag1+lpQR, and AnEn+lpQR
  ########################################################
  for(t in verification)
  {
    ANEN1[t,] <- AnEn[[t]][, k]*McClear[t]
    LAG1LPQR1[t,] <- lag1lpQR[[t]][, k]*McClear[t]
    ANENLPQR1[t,] <- AnEnlpQR[[t]][, k]*McClear[t]
  }
  
  ANEN[[k]] <- ANEN1
  LAG1LPQR[[k]] <- LAG1LPQR1
  ANENLPQR[[k]] <- ANENLPQR1
  
  ########################################################
  # Climatology
  ########################################################
  stn <- Z[,k] # data from 1 station
  ClimECDF <- ecdf(stn) # empirical CDF 
  Climq <- quantile(ClimECDF, tau) # quantiles
  for(t in verification)
  {
    CLIM1[t,] <- Climq * McClear[t]
  }
  CLIM[[k]] <- CLIM1
  
  ########################################################
  # temporal PeEn
  ########################################################
  for(t in verification)
  {
    TMPPEEN1[t,] <- sort(Z[(t-21):(t-1),k]) * McClear[t]
  }
  TMPPEEN[[k]] <- TMPPEEN1
  
  ########################################################
  # Sptial PeEn
  ########################################################
  for(t in verification)
  {
    tmp <- ecdf(Z[t-1,]) # ecdf of all measurements at time t-1
    tmp <- quantile(tmp, tau) # extract quantiles from ECDF
    SPTPEEN1[t,] <- tmp * McClear[t]
  }
  SPTPEEN[[k]] <- SPTPEEN1
  
}# end for k

########################################################
# save data
########################################################
dir.create(file.path(dir0, "Data/Fcst"), showWarnings = FALSE)
setwd(file.path(dir0, "Data/Fcst"))
save("data", "verification", "CLIM", "TMPPEEN", "SPTPEEN", "ANEN", "LAG1LPQR", "ANENLPQR", "time.taken", file = paste0(day, "-", agg, ".RData"))
