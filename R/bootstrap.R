install.packages("boot")
library(boot)
rsq <- function(formula, data, country) {
  d <- data[country,] # allows boot to select sample
  fit <- lm(formula, data=d)
  return(summary(fit)$r.square)
}

results <- boot(data=data, statistic=rsq,
                R=1000, formula=winery~taster_name)
