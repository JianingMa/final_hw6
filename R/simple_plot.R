library(tidyverse)
plot<-function(data,var1){
  plot<-ggplot(data, aes(var1))+geom_bar()
  return(plot)
}

plot(data,data$variety)
