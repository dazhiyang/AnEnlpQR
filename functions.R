#################################################################################
# These functions are written by Dazhi Yang 
# Solar Energy Research Institute of Singapore
# emails: yangdazhi_nus@gmail.com; seryangd@nus.edu.sg
#################################################################################

#################################################################################
# functions to make the QR-lasso forecasts
#################################################################################
QRlasso <- function(Xtra, Xnew, ytra, tau)
{
  # Xtra: training dataset with preselected predictors
  # Xnew: new predictor (1 row only)
  # ytra: training target
  # tau: set of quantiles
  
  # add jitter to the data.frame to avoid the singular design matrix problem
  set.seed(1234)
  Xtra_jittered <- apply(Xtra, 2, jitter)
  # join input and output as a data.frame (this is a requirement of quanreg package)
  data.qr <- data.frame(cbind(Xtra_jittered), y = ytra)
  # fit a quantile regression model with lasso penalty 
  fit <- quantreg::rq(y~., data = data.qr, tau = tau, method = "lasso", lambda = 1)
  # forecasts
  Xnew <- data.frame(Xnew)
  fcst <- predict(fit, newdata = Xnew)
  
  return(fcst)
}

#################################################################################
# compute error metrics
#################################################################################
error <- function(obs, pred, tau)
{
  # PICP
  picp <- mean(sapply(1:length(obs), function(x) ifelse(obs[x] < max(pred[x,]) & obs[x] > min(pred[x,]), 1, 0)))*100
  
  piaw <- mean(sapply(1:length(obs), function(x) max(pred[x,]) - min(pred[x,])))
  
  crps <- mean(crps_sample(y = obs, dat = pred))
  
  # pinball loss
  L <- array(NA, nrow(pred))
  for(x in 1:nrow(pred))
  {
    L1 <- (1-tau)*(pred[x,]-obs[x]) 
    L2 <- tau*(obs[x]- pred[x,]) 
    L[x] <- sum(L1[L1>=0], L2[L2>0])/length(tau)
  }
  pinball <- mean(L)
  
  tibble(PICP = picp, PIAW = piaw, CRPS = crps, pinball = pinball)
}




#################################################################################
# data normalization
#################################################################################
zNorm <- function(x)
{
  x <- (x - mean(x)) / (sd(x)*sqrt((length(x)-1)/length(x))) 
  return(x)
}

#################################################################################
# data normalization
#################################################################################
ED <- function(x, y)
{
  x <- zNorm(x)
  y <- zNorm(y)
  d <- sqrt(sum((x-y)^2))
  d
}

#################################################################################
# mass2
#################################################################################
mass2 <- function(x, query)
{
  require("seismicRoll")
  n <- length(x)
  m <- length(query)
  Q <- zNorm(query)
  #moving standard deviation
  S <- seismicRoll::roll_sd(x, m, align = "left")[1:(n-m+1)]
  S <- S*sqrt((m-1)/m) #get population sd
  Q <- rev(Q)
  Q[(m+1):n] <- 0
  dots <- Re(fft(fft(x)*fft(Q), inverse = TRUE)/n)
  distProfile <- sqrt(2*abs(m-dots[m:n]/S)) #abs counters the (possible) small negative value, due to 0 subtract 0, at the query index
  return(distProfile)
}




