beta.g<-function(data,beta){
  n<-length(data)
  beta.g <- sum((data^beta)*log(data))/sum(data^beta)-1/beta-sum(log(data))/n
  return(beta.g)
}
