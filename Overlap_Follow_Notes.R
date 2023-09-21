## From: https://cran.r-project.org/web/packages/overlap/vignettes/overlap.pdf
library(overlap)
data(kerinci)
head(kerinci)


table(kerinci$Zone)


summary(kerinci$Sps)


range(kerinci$Time)
## time is from 0 to 1
## time-of-capture data from 4 Zones

##time in radians
timeRad <- kerinci$Time * 2 * pi

tig2 <- timeRad[kerinci$Zone == 2 & kerinci$Sps == 'tiger']
densityPlot(tig2, rug=TRUE)
# Fig. 1: Fitted kernel density curve for tigers in Zone 3, using default smoothing parameters
# dinurtal and nocturnal?
#  density is circular distribution
# we use a von Mises kernel, not Gaussian kernel (REVIEW BOTH)
# term ‘rug’: actual data are shown at the foot of Figure 1

## adjust 
# how do i do fig. 2?

## 3 Quantifying overlap
# current method: coefficient of overlapping
# density of min for f(x) and g(x) => delta

## 3.2 Estimatiors
# delta 1 is avg min 

# delta 4  NEED HELP
# delta 5 NEED HELP


# adjust = 0.8 for delta 1
# adjust = 1 for delta 4 (WHAT THIS HOW IT BETTER AT GREATER VALUES??)
# adjust = 4 for delta 5
# {default values}


## 3.4 EX
# kerinci

tig2 <- timeRad[kerinci$Zone == 2 & kerinci$Sps == 'tiger']
mac2 <- timeRad[kerinci$Zone == 2 & kerinci$Sps == 'macaque']
min(length(tig2), length(mac2))
# so 83 times of overlap in Zone 2?



tigmac2est <- overlapEst(tig2, mac2, type="Dhat4")
tigmac2est

## ERROR in overlapEst, correction reload library(overlap)
# result: Dhat4 => 0.4205464

overlapPlot(tig2, mac2, main="Zone 2")
legend('topright', c("Tigers", "Macaques"), lty=c(1,2), col=c(1,4), bty='n')
# Activity curves for tigers and macaques in Zone 2. The coefficient of overlapping equals the area below both curves, shaded grey in this diagram.
# estimate of overlap of 0.42


## CI through bootstrapping bc we need to know a Large sample size
# NOTE: consider bootstrapped CI for overlap in project

tigmac2 <- bootstrap(tig2, mac2, 1000, type="Dhat4")
( BSmean <- mean(tigmac2) )


# the bootstrap values differ from the estimate: this is the bootstrap bias.

bootCI(tigmac2est, tigmac2)
# see 4.2 for definitions 

bootCIlogit(tigmac2est, tigmac2)
# corrections on a logistic scale and back-transforming


## 4.3 Choice of CI method

## 6 Caveats
## 6.1 Pooling data
# beware of time catagorizes

## The average anchoring method
# anchor point in the day (e.g. sunrise)
# will we need to incorperate latitude and longitude to our observations for plot?

# Note Overlap ~ correlation => 0 to 1