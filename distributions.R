
library(tidyverse)
library(scales)

## create your own distribution ####

dist_one <- data.frame(range=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),
                       probs=c(0.2,0.3,0.3,0.1,0.05,0.03,0.01,0.005,0.0025,0.0025))

## cumulate probs
dist_one <- dist_one %>% mutate(cumprob=cumsum(probs),
                                exp=range*probs)
summary(dist_one)
sum(dist_one$range)
sum(dist_one$range)/nrow(dist_one)
sum(dist_one$probs)
sum(dist_one$exp)

## issues with the data because it is a 'synthetic' distribution, with probabilities
## specified, rather than complide
ggplot(dist_one, aes(x=range, y=probs))+geom_col()+
  geom_vline(xintercept=mean(dist_one$range), color='green', size=1)+
  geom_vline(xintercept=median(dist_one$range), color='blue')+
  theme_classic()

## generate random numbers based on these probabilities
rand_one <- runif(n=1000, min=0, max=1)

## when i pick a number from 0 to 1, it has the probabilities of being in one of 
## the buckets according to the probability assigned
## like spinning the roulette wheel that is weighted
## for any given spin, the number will show up according to distribution
## for 100 spins
## what is the chance that a random number between 0 and 1 will be in the range?
## 20% chance it will be less than 0.1

## binomial / bernouli ####
## size = number of trials (number of flips of coin, etc)
## n = number of observations (number of times the trials are performed)
## prob = probability of 'success' on each trial
##  - values can be 0 or 1
##  - 'success' = 1
## 1 coin flip, 1 time, number of heads
rb <- rbinom(n=1, size=1, prob=0.5)
## 1 coin flip, 10 times
rbn <- rbinom(n=10, size=1, prob=0.5)
hist(rbn)
## 10 coin flips, 1 time, number of heads
rb_one <- rbinom(n=1, size=10, prob=0.5) 
## 10 coin flips, 10 times, number of heads each time
rb_two <- rbinom(n=10, size=10, prob=0.5)
hist(rb_two) ## skewed due to randomness in small number of observations
## 10 coin flips, 100 times...
rb_three <- rbinom(n=100, size=10, prob=0.5)
hist(rb_three) ## starting to look more normal, with mean around 5
## 1000 times...
rb_four <- rbinom(n=1000, size=10, prob=0.5)
hist(rb_four) ## getting closer!
## 10,000 times flipping a coin 10 times
## most frequent outcome converges on 5 heads ('successes') - and 5 tails
## different outcomes are distributed pretty evenly outward from there
## with very few of either:
## - 0: all tails
## - 10: all heads
rb_five <- rbinom(n=10000, size=10, prob=0.5)
hist(rb_five) ## irresistable -> although note discrete

## with different odds than coin flip
rb_skew <- rbinom(n=100, size=1, prob=0.2)
hist(rb_skew)
sum(rb_skew) ## number of successes

## possible to combine a bunch of these?

## from ?rbinom help
## compute probability of x being < 45 and < 55 for x binomial(100, 0.5)
sum(dbinom(x=46:54, size=100, 0.5))
### my exploration
dbin <- dbinom(x=46:54, size=100, 0.5)
hist(dbin)
sum(dbin)

## Using "log = TRUE" for an extended range :
n <- 2000
k <- seq(0, n, by = 20)
plot (k, dbinom(k, n, pi/10, log = TRUE), type = "l", ylab = "log density",
      main = "dbinom(*, log=TRUE) is better than  log(dbinom(*))")
lines(k, log(dbinom(k, n, pi/10)), col = "red", lwd = 2)
## END of help

## exponential function (decline) ####
exp <- rexp(n=1000, rate=1)
hist(exp)

geo <- rgeom(n=1000, prob=0.5)
hist(geo)

lno <- rlnorm(n=1000, meanlog=0, sdlog=1)
hist(lno)

## none of which answers my question around compiling my own distribution
## combining bernoulli trials with different probabilities (?)

## quantiles: p = vector of probabilities, size=trials, 
## prob=probability of success on each trial
qbinom(p=c(0.5,0.3,0.2,0.1,0.5), size=100, prob=0.5)

## back to start ####
dist_one
## 20% of the time, we want to generate a number 0-1
## 30% of the time, we want to generate a number 1-2
## 305 of the time, generate 2-3...

## 20% of time, number is 0-0.1
## even chance of number being anywhere between 0 and 1
runif(n=1, min=0, max=1)
rbinom(n=1, size=1, prob=0.2)*0.1

test <- (dist_one[1,1]*dist_one[1,2])+(dist_one[2,1]*dist_one[2,2])+
  (dist_one[3,1]*dist_one[3,2])+(dist_one[4,1]*dist_one[4,2])

## simpler approach
dtwo <- data.frame(range=c(0.3,0.6,1),
                   prob=c(0.5,0.3,0.2))
dtwo
ggplot(dtwo, aes(x=range, y=prob))+geom_col()

## 50% of the time number is between 0 and 0.3
## 30% of time between 0.3 and 0.6
## 20% of time between 0.6 and 1
(0.3*0.5)+(0.6*0.3)+(1*0.2)

## generate a number between 0 and 1
runif(n=1, min=0, max=1)
rbinom(n=1, size=10, prob=0.5)
dunif(x=100, min=0, max=1)
punif(q=0.2, min=0, max=1, lower.tail = FALSE)

## I have 10 sided dice
## 5 sides have 3
## 3 sides have 6
## 1 side has 2
## roll the dice once
dice_all <- NULL
for(d in 1:100){
d <- runif(n=1, min=0, max=1)
if(d<=0.5){
  dice=3
} else if(d<=0.8){
  dice=6
} else {dice=10}
dice_all <- c(dice_all,dice)
print(dice)
}
#print(dice_all)
hist(dice_all)

dist_one
dist_one_all <- NULL
for(d in 1:1000){
  d <- runif(n=1, min=0, max=1)
  if(d<=0.2){
    dice=0.1
  } else if(d<=0.5){
    dice=0.2
  } else if(d<=0.8){
    dice=0.3
  } else if(d<=0.9){
    dice=0.4
  } else if(d<=0.95){
    dice=0.5
  } else if(d<=0.98){
    dice=0.6
  } else if(d<=0.99){
    dice=0.7
  } else if(d<=0.995){
    dice=0.8
  } else if(d<=0.9975){
    dice=0.9
  } else {dice=1}
  dist_one_all <- c(dist_one_all, dice)
}
hist(dist_one_all)

dist_one_all <- NULL
for(d in 1:1000){
  d <- runif(n=1, min=0, max=1)
  if(d<=dist_one$cumprob[1]){
    dice=dist_one$range[1]
  } else if(d<=dist_one$cumprob[2]){
    dice=dist_one$range[2]
  } else if(d<=dist_one$cumprob[3]){
    dice=dist_one$range[3]
  } else if(d<=dist_one$cumprob[4]){
    dice=dist_one$range[4]
  } else if(d<=dist_one$cumprob[5]){
    dice=dist_one$range[5]
  } else if(d<=dist_one$cumprob[6]){
    dice=dist_one$range[6]
  } else if(d<=dist_one$cumprob[7]){
    dice=dist_one$range[7]
  } else if(d<=dist_one$cumprob[8]){
    dice=dist_one$range[8]
  } else if(d<=dist_one$cumprob[9]){
    dice=dist_one$range[9]
  } else {dice=1}
  dist_one_all <- c(dist_one_all, dice)
}
hist(dist_one_all)
dist_one_all_df <- data.frame(dist_one_all)
ggplot(dist_one_all_df, aes(x=dist_one_all))+geom_histogram()

## diversion -> density functions ####
## random normal distribution
normt <- rnorm(n=1000, mean=0, sd=1)
## histogram
hist(normt)
## density function for normt distribution -> gives % probability of any point
normd <- dnorm(x=normt, mean=0, sd=1)
## combine normt with density function values in data frame
normnorm <- data.frame(value=normt,
                       prob=normd)
summary(normnorm)
sum(normnorm$prob)
## plot to see density function: normal curve with probabilities on y-axis
plot(normnorm$prob~normnorm$value)
## histogram for values -> normal shape, as expected
hist(normnorm$value)
## histogram for probs -> rises toward ~0.4
hist(normnorm$prob)
## cumulative probability -> arrange values and add up probs
normnorm <- normnorm %>% arrange(value) %>% mutate(
  cumprob=cumsum(prob)
)
## cumulative distribution curve -> as expected, but why numbers so high?
## it is count -> more values, the higher it goes
plot(normnorm$cumprob~normnorm$value)
sum(normnorm$prob)

## to get cumulative probabilities ####

## probability of value below q, where q = std dev
pnorm(q=0, mean=0, sd=1)
pnorm(q=-1, mean=0, sd=1)
pnorm(q=1, mean=0, sd=1)
## probability of value above q
pnorm(q=-1, mean=0, sd=1, lower.tail = FALSE)
pnorm(q=1, mean=0, sd=1, lower.tail=FALSE)
## probability of value between two sds
## 3 examples showing 68% within 1 sd of mean
pnorm(q=1, mean=0, sd=1)-pnorm(q=-1, mean=0, sd=1)
pnorm(q=-1, mean=0, sd=1, lower.tail=FALSE)-pnorm(q=1, mean=0, sd=1, lower.tail = FALSE)
1-pnorm(q=-1, mean=0, sd=1)-pnorm(q=1, mean=0, sd=1, lower.tail=FALSE)
## 95% within 2 sd of mean
pnorm(q=2, mean=0, sd=1)-pnorm(q=-2, mean=0, sd=1)

## gives std dev for a probability
qnorm(p=0.1586553, mean=0, sd=1)
qnorm(p=0.5, mean=0, sd=1)
qnorm(p=0.95, mean=0, sd=1)
qnorm(p=.975, mean=0, sd=1)
qnorm(p=0.68, mean=0, sd=1)

## DICE ####
## for throws of a single die
## - use sample 
## - x = vector of numbers to choose from
## - size = number of items to select (similar to how 'n' is used elsewhere)
## - replace = TRUE because you can roll the same number more than once
die_one <- sample(x=1:6, size=1000, replace=TRUE)
hist(die_one)
die_two <- sample(x=1:6, size=1000, replace=TRUE)
hist(die_two)

dice_roll <- data.frame(die_one,
                        die_two)
dice_roll <- dice_roll %>% mutate(
  combo=die_one+die_two
)

## histogram, with breaks set for x axis
ggplot(dice_roll, aes(x=combo))+
  geom_histogram()+
  scale_x_continuous(breaks = 2:12, expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  theme_classic()

## convert to density plot, although:
## a) density % are too high
## b) add up to way more than 1
ggplot(dice_roll, aes(x=combo))+
  geom_histogram(aes(y=..density..))+
  scale_x_continuous(breaks = 2:12, expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  theme_classic()
## this works, though:
ggplot(dice_roll, aes(x=combo))+
  geom_density()+
  scale_x_continuous(breaks = 2:12, expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  theme_classic()

## tables
table(dice_roll$combo)
prop.table(table(dice_roll$combo))
