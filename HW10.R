## Setup
library(tidyverse)
library(car)

########### Objective 1 ########### 

## Reading in seed dispersal data
seed.distances <- read.csv("seed.summary.trans_siteinfo.csv")


## my research looks at seed delivery in rugged terrain, so I am 
## going to look at the median seed delivery distance as a function 
## of terrain ruggedness index (TRI, measured on a 30m x 30m resolution).
## My study was designed to have 12 replicates, but three dispersal 
## transects collected no seeds, so I'm using the nine transects that 
## trapped seeds. This is as close as I can get to "sufficient numbers 
## of paired observations"


#### making lm object relating median dispersal distance to TRI
seeds.by.tri <- lm(med.disp.dist.pico ~ max.TRI.30, data = seed.distances)


#### evaluating model assumptions

#visualizing data to check for linearity
plot(med.disp.dist.pico ~ max.TRI.30, data = seed.distances)
abline(seeds.by.tri)
# it's not perfectly linear, but I think assuming linearity would still
# be valid

## Normality
qqPlot(seeds.by.tri) 
# normality assumption seems to be met because comparisons between the sample 
# quantiles and known quantiles appear to have a slope of 1

## Homoscedasticity
plot(seeds.by.tri, which = 1) 
# there is no clear trend in the variance of residuals, suggesting
# the homoscedasticity assumption is met. 


#### Generating predictions
newdata = data.frame(max.TRI.30 = c(median(seed.distances$max.TRI.30), (max(seed.distances$max.TRI.30)*0.95)))

pred <- predict(seeds.by.tri,newdata, interval = "prediction")

pred.data <- cbind(newdata, pred)

## at the 95th percentile, the prediction interval is slightly wider
## than it was at the median

########### Objective 2 ########### 

# generating random linear-ish data
x <- seq(0.01, 1, by = 0.01)
set.seed(10)
error.1 <- rlnorm(100)
y1 <- 2.5*x +10 + error.1

lnorm.lm <- lm(y1~x)
lnorm.lm$coefficients
# slope estimate is 3.494874, which is higher than the true slope
# intercept estimate is 10.8377, which is slightly higher than the true intercept



## generating a new random log-normal error term
set.seed(20)
error.2 <- rlnorm(100)
y2 <- 2.5*x +10 + error.2

lnorm.lm2 <- lm(y2~x)
lnorm.lm2$coefficients
# slope estimate is 3.236801, which is also higher than the true slope
# intercept estimate is 11.191 is also slightly higher than the true intercept

## 95% prediction intervals

# model 1
pred.lnorm <- predict(lnorm.lm, data.frame(x),  interval = "prediction")

prediction.lnorm <- data.frame(x,y, pred.lnorm)

# adding a column for whether the y value falls within the 95% prediction interval
prediction.lnorm$y95 <- ifelse(prediction.lnorm$y > prediction.lnorm$lwr &
                                 prediction.lnorm$y < prediction.lnorm$upr, 
                               TRUE, FALSE)
sum(prediction.lnorm$y95== T)
# 95/100 y values are within the prediction interval


# model 2
pred.lnorm2 <- predict(lnorm.lm2, data.frame(x),  interval = "prediction")

prediction.lnorm2 <- data.frame(x,y, pred.lnorm2)

# adding a column for whether the y value falls within the 95% prediction interval
prediction.lnorm2$y95 <- ifelse(prediction.lnorm2$y > prediction.lnorm2$lwr &
                                 prediction.lnorm2$y < prediction.lnorm2$upr, 
                               TRUE, FALSE)
sum(prediction.lnorm2$y95== T)
# 95/100 y values are within the prediction interval

## since y values are still withing the 95% prediction interval, it suggests that 
## the estimated uncertainty is still pretty close to the true uncertainty, 
## even if the residuals are not normally distributed. 