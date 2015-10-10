library(ggplot2)
library(qualityTools)


rsample <- function(typ, lam=1, rep=1, exp.max=0, poi.max=1, gam.alpha=1){ 
  # the lam parameter is shared by exp,poi,gam distribution.
  # "exp." means this parameter is used only when generating exponetial random samples
  # so does the "poi." or "gam."
  # parameter:
  #   typ: the type of distribution, c("exp", "poi", "gam")
  #   lam: the common parameter in exp. poi. and gamma distribution.
  #   rep: length of samples.
  #   exp.max: threshold of sum of exponential random samples, when getting expo. samples.
  #   poi.max: length of given time interval in a poisson process.
  #   gam.alpha: the first parameter of gamma distribution, number of exponential variables.
  # return:
  #   a sample vector
  if(rep<=0){warning("rep must be integer greater than 0")}
  if(typ=="exp"){ # exp: rep, lam, exp.max
    if(exp.max<=0){ 
      rt <- rexp(rep,lam)
    } else {
      rt <- rexp.max(exp.max, lam)
    }
  } else if(typ=="poi"){ # poi: rep, lam, poi.max
    if(poi.max<=0){warning("poi.max should be greater than zero")}
    # poisson variable is length of exp. with sum less than certain threshold.
    rt <- NULL
    for(i in 1:rep){
      tmp <- rexp.max(poi.max, lam)
      len <- length(tmp)
      rt <- c(rt, len)
    }
  } else if(typ=="gam"){ # gam: rep, lam, gam.alpha
    if(gam.alpha<=0){warning("gam.alpha should be greater than zero")}
    # gamma variable is sum of exp. with certain length
    rt <- NULL
    for(i in 1:rep){
      rt <- c(rt, sum(rexp(gam.alpha, lam)))
    }
  } else {warning("wrong type")}
  return(rt)
}

# I don't use this function directly. 
# it is only invoked in rsample()
rexp.max <- function(max, lam){
  t=0
  a=NULL
  while(t < max){
    inter <- rexp(1,lam)
    a <- c(a, inter)
    t <- t + inter
  }
  rt <- a[1:(length(a)-1)]
  return(rt)
}

rsample("nba")
rsample("exp", rep=5, lam=-2)
rsample("exp", rep=5, lam=2, exp.max=-4)
rsample("exp", rep=5, lam=2, exp.max=10)
rsample("exp", rep=-4, lam=3)
rsample("exp", rep=-4, lam=3, exp.max=3)
rsample("poi", rep=5, lam=2, poi.max=3)
rsample("poi", rep=5, lam=2, poi.max=-3)
rsample("gam", rep=5, lam=2, gam.alpha=-4)
rsample("gam", rep=5, lam=2, gam.alpha=1.3)
rsample("gam", rep=5, lam=2, gam.alpha="HAHAHA")
