
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

##4
ggplot(DF, aes(x=sampleMeans, fill =sampleSize))+
  geom_density(alpha=0.35) + facet_wrap(~sampleSize) + theme_dark()
##4
data<- read.csv("https://raw.githubusercontent.com/JackStat/PracticalDataScience/master/data/winemag-data-130k-v2.csv")
names(data)
grouping <- function(data,vars,groupings){

  count<-data%>%group_by_at(vars(one_of(groupings)))
  return(count)
  }
grouping(data,points,price)

grouping<-function(data,vars){
  vars <- enquo(vars)
 count<- data %>%

   group_by(!! vars) %>%
  summarise(
    count=n()
  )
 return(count)
}

grouping(data,variety)
##5
##1 function ideas
sampleDist <- function(samples = 100, sample_size=200) {
  sampleMeans<- rep(NA, samples)
  for (i in 1:samples ) {
    x <-rnorm(10, mean =1, sd=6)
    sampleMeans[i] = mean(x)
  }
  return(sampleMeans)
}
##2 power analysis
install.packages("pwr")
library(pwr)
power <- function(M1,M2,S1,S2){

co.d=(M1 - M2)/sqrt(((S1^2) + (S2^2))/2)
result<-pwr.t.test(
  n = NULL,
  d = co.d,
  sig.level = 0.05,
  power = 0.80,
  type = "two.sample",
  alternative = "two.sided"

)
return(result)
}
power(66,65,5,6)
##3 Bootstrap
install.packages("boot")
library(boot)
rsq <- function(formula, data, country) {
  d <- data[country,] # allows boot to select sample
  fit <- lm(formula, data=d)
  return(summary(fit)$r.square)
}

results <- boot(data=data, statistic=rsq,
                R=1000, formula=winery~taster_name)
##4
beta.g<-function(data,beta){
  n<-length(data)
  beta.g <- sum((data^beta)*log(data))/sum(data^beta)-1/beta-sum(log(data))/n
  return(beta.g)
}
##5
mle.theta <- function(data,beta){
  n<-length(data)
  theta=(sum((data^beta)/n))^(1/beta)
  return(theta)
}
##6
sum_up<-function(a,b){
  x<- a +b
  return(x)
}


## 7 plot
library(tidyverse)
plot<-function(data,var1){
  plot<-ggplot(data, aes(var1))+geom_bar()
  return(plot)
}

plot(data,data$variety)

##8
weibull<- function(x,k,lamda){
  f=(k/lamda)*((x/lamda)^(k-1))*exp(-(x/lamda)*k)
  return(f)
}

## testthat
install.packages("testthat")
library(testthat)
?testthat
a<-9
test1<-expect_that(a,is_less_than(10))
test2<-expect_lt(a,10)

string <- "Apple"
expect_match(string, "Testing")

expect_equal(10, 11)
