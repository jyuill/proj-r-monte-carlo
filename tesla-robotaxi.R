## Valuing Tesla Robotaxi program with Monte Carlo
## based on info here:
## https://www.teslarati.com/tesla-network-robotaxi-fleet-details-elon-musk/

library(tidyverse)

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

## annual miles driven
miles <- rnorm(n=ss, mean=90000, sd=20000)
hist(miles)

## revenue miles, as %
revmiles <- rnorm(n=ss, mean=0.5, sd=0.25)
hist(revmiles)

## MONTE CARLO SIMULATION OF PROFIT
## you have 4 vectors of same length -> you can do calculations on them
net <- (grosspermile-costpermile)*miles*revmiles
hist(net)

## summary of net
summary(net)
sd(net)

## calculate 95% confidence intervals :)