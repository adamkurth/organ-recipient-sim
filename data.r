rm(list=ls())

d <- 10 # donors
r <- 10 # recipients

# generate random data
set.seed(1)
k <- 3 # features of interest (e.g. age, blood type, etc.)

data.d <- data.frame(
  donor = 1:d,
  value = rnorm(d)
)
data.r <- data.frame(
    recipient = 1:r,
    eval = rnorm(r) #
)


data.r <- data.frame(
  donor = sample(1:d, 100, replace = TRUE),
  recipient = sample(1:r, 100, replace = TRUE),
  value = rnorm(100)
)
