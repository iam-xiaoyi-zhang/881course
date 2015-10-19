# haviland's code
#library(ggplot2)
#raw <- read.csv("raw data/L-00-01.txt",skip=2,stringsAsFactors=F,header=T)
#raw[rwa=="T"]


# read data
year <- c("00","01","02","03","04")
month <- c("01","02","03","04","05","06","07","08","09","10","11","12")
data <- list()
count <- 0
for(y in year){
  for(m in month){
    file <- paste("L-",y,"-",m,".csv",sep="")
    print(file)
    data[[(paste(y,m,sep=""))]] <- read.csv(paste("rain_csv/",file,sep=""))
    count <- count + 1
  }
}

# clean the data 
any(data$'0001'=='----')
any(data$'0001'=='T   ')
length(data)
for(i in c(1:length(data))){
  tmp <- data[[i]]
  for(j in c(1:length(tmp))){
    tmp[,j] <- as.vector(tmp[,j])
    tmp[,j][strtrim(tmp[,j],1) %in% c('-','M')] <- 0
    tmp[,j][strtrim(tmp[,j],1)=='T'] <- 999
    tmp[,j] <- as.numeric(tmp[,j])
  }
  data[[i]] <- tmp
}

# get rid of DT column
for(i in c(1:length(data))){
  data[[i]][,1] <- NULL
}

# put all data in one vector
line <- NULL
for(i in c(1:length(data))){
  tmp <- data[[i]]
  for(j in c(1:length(tmp[,1]))){
    line <- c(line, tmp[j,])
  }
}
line <- unlist(line)
names(line) <- NULL

# get storm data
stormline <- NULL
storm <- NULL
Train <- 0.001
for(i in c(1:length(line))){
  if(line[i]>0){ # the storm begins !!!
    if(line[i]==999){
      storm <- c(storm, Train)
    } else {
      storm <- c(storm, line[i])
    }
  } else { # line[i]==0 # the storm ends/have not started
    if((i==1)||(line[i-1]>0)){ # it WAS raining
      stormline <- c(stormline, sum(storm))
      #print(c("stormline",sum(storm)))
      storm <- 0
    }
  }
  #print(c(line[i],storm))
}
length(stormline)
cdf <-ecdf(stormline)
densityPlot(stormline)


# Estimation

# 1
# Assuming that the rain gauge data are gamma distributed
# Get the estimates of gamma parameters and then test it by plotting and CI

# MME of gamma distribution parameters
alpha <- mean(stormline)^2/var(stormline)  # 0.1846
lambda <- mean(stormline)/var(stormline)   # 1.1204
df <- data.frame(s=stormline)

# make a plot
library(ggplot2)
ggplot(data=df, aes(x=s)) +
  geom_histogram(aes(y=..density..), binwidth=.15, color="black", fill="white") +
  geom_density()+
  stat_function(fun=dgamma, args=list(shape=alpha, scale=lambda), color="green")

# bootstrap of MME
# If we can get a comfortable confidence interval, 
# then we can conclude that the storm data is gamma distributed
bootn <- 1000
m1boot <- NULL
m2boot <- NULL
len <- length(stormline)
for(i in c(1:bootn)){
  boot <- sample(stormline, len, replace=TRUE)
  m1boot <- c(m1boot, mean(boot))
  m2boot <- c(m2boot, mean(boot^2))
}
alpha_boot <- (m1boot^2)/(m2boot - m1boot^2)
lambda_boot <- m1boot / (m2boot - m1boot^2)
sd_alpha_boot <- sd(alpha_boot)
sd_lambda_boot <- sd(lambda_boot)
# CI of alpha
a.ci.nr <- c(alpha-2*sd_alpha_boot, alpha+2*sd_alpha_boot) 
a.ci.pr <- c(quantile(alpha_boot,.025), quantile(alpha_boot,.975))
a.ci.pv <- c(2*alpha-quantile(alpha_boot,.975), 2*alpha-quantile(alpha_boot,.025))
# CI of lambda
l.ci.nr <- c(lambda-2*sd_lambda_boot, lambda+2*sd_lambda_boot)
l.ci.pr <- c(quantile(lambda_boot,.025), quantile(lambda_boot,.975))
l.ci.pv <- c(2*lambda-quantile(lambda_boot,.975), 2*lambda-quantile(lambda_boot,.025))
a.ci.nr
a.ci.pr
a.ci.pv
l.ci.nr
l.ci.pr
l.ci.pv

# Given the confidence interval of parameters, we cannot reject the assumption that
# the rain gauge data are gamma distributed.

# MLE of gamma distribution
n <- length(stormline)
likelihoodf <- function(theta) {-(n*theta[1]*log(theta[2])-n*lgamma(theta[1])+(theta[1]-1)*sum(log(stormline))-theta[2]*sum(stormline))}
max.likelihood <- nlminb(start=c(0.12,1.8), obj = likelihoodf)

aMLE <- max.likelihood$par[1]
lMLE <- max.likelihood$par[2]
aMLE
lMLE




# 2
# Assuming that the rain gauge data are exponential distributed
# get the estimate of exponential parameter and make the CI and plot
lambda2 <- 1/mean(stormline)
lambda2

# plot
ggplot(data=df, aes(x=s)) +
  geom_histogram(aes(y=..density..), binwidth=.15, color="black", fill="white") +
  geom_density()+
  stat_function(fun=dexp, args=list(lambda), color="green")

# From the plot, we can infer that the storm data is not exponential distributed.

# we can still use the bootstrap to check CI
bootn <- 1000
lboot <- NULL
len <- length(stormline)
for(i in c(1:bootn)){
  boot <- sample(stormline, len, replace=TRUE)
  lboot <- c(m1boot, 1/mean(boot))
}
sd_lboot <- sd(lboot)
# CI of alpha
# CI of lambda
l.ci.nr <- c(lambda2-2*sd_lboot, lambda2+2*sd_lboot)
l.ci.pr <- c(quantile(lboot,.025), quantile(lboot,.975))
l.ci.pv <- c(2*lambda2-quantile(lboot,.975), 2*lambda2-quantile(lboot,.025))

l.ci.nr
l.ci.pr
l.ci.pv

# after comparing three confidence interval and the estimate of lambda
# we have enough evidence to reject the assumption that storm data is 
# not exponential distributed.