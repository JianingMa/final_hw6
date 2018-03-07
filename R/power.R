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
