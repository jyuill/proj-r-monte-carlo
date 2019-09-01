## Simulating Hypothesis Testing

library(tidyverse)

# MoM chg in sales in mkts with no ads
no_ads <- c(-2.8,
                 1.9,
                 -7.9,
                 8.5,
                 4.1,
                 3,
                 3.3,
                 1.7,
                 6.6,
                 7,
                 3.4,
                 5,
                 8.2,
                 6.6,
                 4.1,
                 10.1,
                 -3.5,
                 -1.2,
                 10.6,
                 9.9
                 )
mean(no_ads)
## MoM chg in sales in mkts with ads
ads <- c(6.6,
       9.8,
       8.6,
       -1.3,
       4.9,
       14.1,
       11.4,
       13.9,
       6.6,
       -1.1)
mean(ads)
## point lift from ads
mean(ads)-mean(no_ads)
## % lift from ads
mean(ads)/mean(no_ads)-1
(mean(ads)-mean(no_ads))/mean(no_ads)

## combine data sets to create single population
combined <- c(no_ads, ads)
mean(combined)

## Objective:
## - simulate random selection from overall population to determine
##   how likely it is to get a difference 
##   equal or greater than point lift from ads

## Sim
mean_diff_all <- NULL
for(s in 1:1000){
sim_ads <- sample(x=combined, size=10, replace = FALSE)
sim_no_ads <- sample(x=combined, size=20, replace = FALSE)

mean_diff <- mean(sim_ads)-mean(sim_no_ads)
mean_diff_all <- c(mean_diff_all, mean_diff)
}

## summary stats
mean(mean_diff_all)
sd(mean_diff_all)
## viz
par(mfrow=c(2,1))
hist(mean_diff_all)
hist(rnorm(n=1000, mean=mean(mean_diff_all), sd=sd(mean_diff_all)))

## Probability of getting the experiment difference or greater?
pnorm(q=mean(ads)-mean(no_ads), mean=mean(mean_diff_all), 
      sd=sd(mean_diff_all),
      lower.tail=FALSE)
result <- pnorm(q=mean(ads)-mean(no_ads), mean=mean(mean_diff_all), 
                sd=sd(mean_diff_all),
                lower.tail=FALSE)
if(result<0.05){
  print(paste0("Success! You have statistical signficance. There is only ",
               result, " chance of getting this result randomly."))
} else {
  print("Sorry, not statistically significant!")
}
## Where is the 95% cut-off?
qnorm(p=0.95, mean=mean(mean_diff_all), 
      sd=sd(mean_diff_all))
cutoff <- qnorm(p=0.95, mean=mean(mean_diff_all), 
                sd=sd(mean_diff_all))
paste0("Anything above ",cutoff," has less than 5% chance of occurring randomly.")
