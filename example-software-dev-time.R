## Simulation example: software development times

## Scenario:
## 10 parts to a software application
## each developed independently, in parallel
## each takes on ave 6 months to build
## question: what is expected time of completion for the whole application?

## common sense says 6 months (expected time, based on averages of each component)

## actual completion time is based on the longest of the 10 parts
## so the expected total completion is the expected LONGEST time for any part

## MODEL the PROCESS with SIMULATION
library(tidyverse)
library(scales)

## parameters for distribution of software dev time
## normal dist with mean software dev time and std dev as below
msdt <- 6
sdsdt <- 2

## 10 software components: software dev time example
sdte <- rnorm(n=10, mean=msdt, sd=sdsdt)
sdte
## mean of example above
mean(sdte)
## max of example above - this will be the actual total completion time
max(sdte)

## Running simulation multiple times will provide estimate of 
## expected total time, based on max

## need data frame with each software component in cols

## first, data frame with components in rows
df_sdt <- data.frame(sc=c(1:10),
                     sdt=sdte)
### end example ////////////

### SIMULATIONS
## use loop for multiple simulation examples
df_sdtall <- data.frame()
msdt <- 6
sdsdt <- 2
parts <- 10
sims <- 100
for(s in 1:sims){
  sdte <- rnorm(n=10, mean=msdt, sd=sdsdt)
  
  df_sdt <- data.frame(sim=s,
                       sc=c(1:parts),
                       sdt=rnorm(n=parts, mean=msdt, sd=sdsdt))
  df_sdtall <- bind_rows(df_sdtall, df_sdt)
}

## summary stats by sim
df_sdtsim <- df_sdtall %>% group_by(sim) %>% summarize(mean_time=mean(sdt),
                                                    max_time=max(sdt))

## chart for mean
ggplot(df_sdtsim, aes(x=sim, y=mean_time))+geom_col()+
  geom_hline(yintercept=mean(df_sdtsim$mean_time))+theme_classic()

## chart for max
ggplot(df_sdtsim, aes(x=as.factor(sim), y=max_time))+geom_col()+
  geom_hline(aes(yintercept=mean(df_sdtsim$max_time), color='avg max'))+
  geom_hline(aes(yintercept=mean(df_sdtsim$mean_time), color='avg mean'))+
  scale_y_continuous(expand=c(0,0))+
  theme_classic()

## histograms for mean and max
ggplot(df_sdtsim, aes(x=mean_time))+geom_histogram()+theme_classic()
ggplot(df_sdtsim, aes(x=max_time))+geom_histogram()+theme_classic()

## combined histogram
ggplot(df_sdtsim, aes(x=mean_time))+geom_histogram()+
  geom_histogram(aes(x=max_time))+
  theme_classic()