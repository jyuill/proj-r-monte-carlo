## Valuing Tesla Robotaxi program with Monte Carlo
## based on info here:
## https://www.teslarati.com/tesla-network-robotaxi-fleet-details-elon-musk/

library(tidyverse)
library(scales)

## CALCULATIONS WITHOUT SIMULATION ####
costpermile_ave <- 0.18
grosspermile_ave <- 1.30
miles_ave <- 90000
revmiles_ave <- 0.5

## calculated earnings
net_ave <- (grosspermile_ave-costpermile_ave) * miles_ave * revmiles_ave

## sample size
ss <- 1000

## cost per mile distribution
costpermile <- rnorm(n=ss, mean=0.18, sd=0.04)
hist(costpermile)

## revenue profit per mile
## gross is estimated at $0.65 with car empty 50% of time
## profit per = ($0.65*2)
grosspermile <- rnorm(n=ss, mean=(0.65*2), sd=0.5)
hist(grosspermile)
## poisson? no - poisson is not continuous
#gpm <- rpois(n=ss, lambda = (0.65*2))
#hist(gpm)

## annual miles driven
miles <- rnorm(n=ss, mean=90000, sd=20000)
hist(miles)

## revenue miles, as %
## need to adjust distribution to not generate values below 0 or above 1
revmiles <- rnorm(n=ss, mean=0.5, sd=0.15)
hist(revmiles)
summary(revmiles)

## OR: create distribution with min and max cut-offs
## will inflace probabilities of either min or max, at expense of center
revmile_new <- NULL
for(r in 1:ss){
  revmile <- rnorm(n=1, mean=0.5, sd=0.25)
  revmile <- max(0, revmile)
  revmile <- min(1, revmile)
  revmile_new <- c(revmile_new,revmile)
}
hist(revmile_new)
summary(revmile_new)
## compare with full normal dist, same mean and sd
revmile_alt <- rnorm(n=ss, mean=0.5, sd=0.25)
hist(revmile_alt)
summary(revmile_alt)

## MONTE CARLO SIMULATION OF PROFIT
## you have 4 vectors of same length -> you can do calculations on them
net <- (grosspermile-costpermile)*miles*revmiles
hist(net)
summary(net)
sim_df <- data.frame(grosspermile,
                     costpermile,
                     miles,
                     revmiles,
                     net)
net_df <- data.frame(net)
ggplot(sim_df, aes(x=net))+geom_histogram()+
  geom_vline(xintercept=mean(net_df$net))+
  geom_vline(xintercept=mean(net_df$net)-sd(net_df$net))+
  geom_vline(xintercept=mean(net_df$net)+sd(net_df$net))+
  scale_x_continuous(labels = comma, expand=c(0,0))+
  scale_y_continuous(labels = comma, expand=c(0,0))+
  theme_classic()
## summary of net
summary(net)
sd(net)

## calculate 95% confidence intervals :)