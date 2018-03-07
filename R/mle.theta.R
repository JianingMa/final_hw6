mle.theta <- function(data,beta){
  n<-length(data)
  theta=(sum((data^beta)/n))^(1/beta)
  return(theta)
}
