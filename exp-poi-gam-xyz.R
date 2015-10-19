library(ggplot2)
library(qualityTools)

#make set.seed(50) at the beginning to make sure result are same as others

set.seed(50)
rsample <- function(typ, lam=1, rep=1, exp.max=0, poi.max=1, gam.alpha=1){ 
  # parameter:
  #   typ: the type of distribution, c("exp", "poi", "gam")
  #   lam: the common parameter in exp. poi. and gamma distribution.
  #   rep: length of samples.
  #   exp.max: threshold of sum of exponential random samples, when getting expo. samples.
  #   poi.max: length of given time interval in a poisson process.
  #   gam.alpha: the first parameter of gamma distribution, number of exponential variables.
  # return:
  #   a sample vector
  # Note: the lam parameter is shared by exp,poi,gam distribution.
  #       "exp." means this parameter is used only when generating exponetial random samples
  #       so does the "poi." or "gam."
  if(rep<=0){warning("rep must be integer greater than 0")}
  if(typ=="exp"){ # exp: rep, lam, exp.max
    if(exp.max<=0){ 
      ret <- rexp(rep,lam)
    } else {
      ret <- rexp.max(exp.max, lam)
    }
  } else if(typ=="poi"){ # poi: rep, lam, poi.max
    if(poi.max<=0){warning("poi.max should be greater than zero")}
    # poisson variable is length of exp. with sum less than certain threshold.
    ret <- NULL
    for(i in 1:rep){
      tmp <- rexp.max(poi.max, lam)
      len <- length(tmp)
      ret <- c(ret, len)
    }
  } else if(typ=="gam"){ # gam: rep, lam, gam.alpha
    if(gam.alpha<=0){warning("gam.alpha should be greater than zero")}
    # gamma variable is sum of exp. with certain length
    ret <- NULL
    for(i in 1:rep){
      ret <- c(ret, sum(rexp(gam.alpha, lam)))
    }
  } else {warning("wrong type")}
  
  meanret <- mean(ret)
  print(meanret)
  df <- data.frame(ret=ret, meanret=meanret)
  p <- ggplot(data=df)
  p <- p + geom_density(aes(x=ret), binwidth=1) + xlab(typ) + geom_vline(aes(xintercept=meanret))
  print(p)
  return(ret)
}

# I don't use this function directly. ggplot() + geom_density(aes(x=dat), binwidth=1)
# it is only invoked in rsample()
rexp.max <- function(max, lam){
  t=0
  a=NULL
  while(t < max){
    inter <- rexp(1,lam)
    a <- c(a, inter)
    t <- t + inter
  }
  ret <- a[1:(length(a)-1)]
  return(ret)
}

#Comment, he set up it by using density, so the result will be 100 time less than mine

dat <- rsample("exp", rep=1000, lam=2)
dat <- rsample("exp", rep=1000, lam=2, exp.max=10)
dat <- rsample("poi", rep=1000, lam=2, poi.max=3)
dat <- rsample("gam", rep=1000, lam=2, gam.alpha=4)

#otherwise! it is a prefect code! XD


