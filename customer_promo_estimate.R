## ESTIMATING POTENTIAL RETURNS ON A CUSTOMER PROMOTION

## load libraries
library(tidyverse)
library(scales)

## get data - prepared in advance
cust <- read_csv('data/customer_promo.csv')

## examine customer data
str(cust)

## identify variables for estimating: the unknowns:
## Exp_promo_convert: expecting number of customers to convert via promo
## ARPU_Future: expected ARPU for customers during time period
## ARPU_promo: expected spend rate for customers who converted via promo

## Exp_promo_convert distribution
sd_est <- 0.1

## works but applies same random var to each item
cust_epc <- cust %>% mutate(
  epc_est=Exp_promo_convert*rnorm(mean=1, sd=sd_est, n=1)
)

## works but loop
cust_epc2 <- cust
for(s in 1:nrow(cust)){
  cust_epc2$epc_est[s] <- cust_epc2$Exp_promo_convert[s]*rnorm(mean=1, sd=sd_est, n=1)
}

cust_est <- cust
## create function and use apply
fdist <- function(x) {
  x*rnorm(mean=1, sd=sd_est, n=1)
}
## apply function to each row to create new variable with random normal dist
cust_est$epc_est <- apply(cust_est[,'Exp_promo_convert'], MARGIN=1, FUN=fdist)
## set C3 prev title to 1
cust_est[cust_est$Prod=='C3 (prev title pattern)','epc_est'] <- 1
cust_est$arpu_future_est <- apply(cust_est[,'ARPU_Future'], MARGIN=1, FUN=fdist) 
cust_est$arpu_promo_est <- apply(cust_est[,'ARPU_promo'], MARGIN=1, FUN=fdist)
cust_est <- cust_est %>% mutate(
  rev_est=Customers*epc_est*arpu_future_est*arpu_promo_est
)

sum(cust_est$Revenue)
sum(cust_est$rev_est)

## use above process and create a loop for multiple simulations
## add new function with wider spread for some variables
sd_estw <- 0.2
fdistw <- function(x) {
  x*rnorm(mean=1, sd=sd_estw, n=1)
}
cust_est_sim <- data.frame()
sims <- 2000
for(sim in 1:sims){
  ## apply function to each row to create new variable with random normal dist
  cust_est$epc_est <- apply(cust_est[,'Exp_promo_convert'], MARGIN=1, FUN=fdist)
  ## set C3 prev title to 1
  cust_est[cust_est$Prod=='C3 (prev title pattern)','epc_est'] <- 1
  cust_est$arpu_future_est <- apply(cust_est[,'ARPU_Future'], MARGIN=1, FUN=fdistw) 
  cust_est$arpu_promo_est <- apply(cust_est[,'ARPU_promo'], MARGIN=1, FUN=fdist)
  cust_est <- cust_est %>% mutate(
    rev_est=Customers*epc_est*arpu_future_est*arpu_promo_est
  )
  cust_est$sim <- sim
  cust_est_sim <- bind_rows(cust_est_sim, cust_est)
}

## review results of sim
## total revenue
rev_sim <- cust_est_sim %>% group_by(sim) %>% summarize(rev_est=sum(rev_est))
## visualize distribution of revenue
ggplot(rev_sim, aes(x=rev_est))+geom_histogram()+
  scale_x_continuous(labels=comma)

summary(rev_sim$rev_est)
