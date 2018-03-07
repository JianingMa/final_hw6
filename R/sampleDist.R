sampleDist <- function(samples = 100, sample_size=200) {
  sampleMeans<- rep(NA, samples)
  for (i in 1:samples ) {
    x <-rnorm(10, mean =1, sd=6)
    sampleMeans[i] = mean(x)
  }
  return(sampleMeans)
}
