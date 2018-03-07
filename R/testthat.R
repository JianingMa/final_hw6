install.packages("testthat")
library(testthat)
?testthat
a<-9
test1<-expect_that(a,is_less_than(10))
test2<-expect_lt(a,10)

string <- "Apple"
expect_match(string, "Testing")

expect_equal(10, 11)
