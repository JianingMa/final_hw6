##4
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
