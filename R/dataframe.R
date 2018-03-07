
library(tidyverse)
library(dplyr)
##3
sampleDist <- function(samples = 100, sample_size=200) {
  sampleMeans<- rep(NA, samples)
  for (i in 1:samples ) {
    x <-rnorm(10, mean =1, sd=6)
    sampleMeans[i] = mean(x)
  }
  return(sampleMeans)
}

DF <- data.frame(
  s100=sampleDist(250,100)
  ,s150 = sampleDist(250, 150)
  ,s250 = sampleDist(250, 250)
  , s500 = sampleDist(250, 500)
  , s1000 = sampleDist(250, 1000)
  , s5000 = sampleDist(250, 5000)
) %>%
  gather(sampleSize,sampleMeans)

ggplot(DF, aes(x=sampleMeans, fill =sampleSize))+
  geom_density(alpha=0.35) + facet_wrap(~sampleSize)
