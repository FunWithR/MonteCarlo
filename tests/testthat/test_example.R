
context("example")

test_that("this is my first test",{
  expect_equal(1+1,2)
})


# set.seed(1234)
#
# R <- c(10,20)
# P <- 1:2
# 
#funkyfunc <- function(R,P){
#  #y <- rnorm(1)
#  y <- 1
#  nn <- R*P + R + y
#  return(list("nn"=nn))
#}

#param_list <- list("R"=R, "P"=P)
#reps <- 5
#MCsim2 <- MonteCarlo(func=funkyfunc, nrep=reps, param_list=param_list, ncpus=1)
#summary(MCsim2)
#MCsim2$results

#myFun <- function(a,b){
#  return(list(res = a+b))
#}
#MonteCarlo(func = myFun, nrep = 100, param_list = list(a = 1, b = 1))"
