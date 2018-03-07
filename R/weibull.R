weibull<- function(x,k,lamda){
  f=(k/lamda)*((x/lamda)^(k-1))*exp(-(x/lamda)*k)
  return(f)
}
