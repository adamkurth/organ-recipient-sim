rm(list=ls())
cwd <- getwd()

# Parameters
d <- 10  # number of donors
r <- 10  # number of recipients
k <- 3   # number of features

# Constraints
age.min <- 2
age.max <- 80

# Helper functions
generate.blood.type <- function(n) {
  blood.types <- c("A", "B", "AB", "O")
  blood.type.prob <- c(0.3, 0.2, 0.1, 0.4)
  sample(blood.types, n, replace=TRUE, prob=blood.type.prob)
}

generate.age <- function(n, is.donor) {
  if(is.donor) {
    # Donors are more likely to be younger
    alpha <- 2
    beta <- 3
    round(rbeta(n, shape1 = alpha, shape2 = beta) * (age.max - age.min) + age.min)
  } else {
    # Recipients have a more uniform distribution, slightly skewed towards older ages
    alpha <- 2
    beta <- 1.5
    round(rbeta(n, shape1 = alpha, shape2 = beta) * (age.max - age.min) + age.min)
  }

}
# hist(generate.age(10000, TRUE), breaks=seq(0, 100, by=5), main="Donor Age Distribution", xlab="Age", ylab="Frequency")
# hist(generate.age(10000, FALSE), breaks=seq(0, 100, by=5), main="Donor Age Distribution", xlab="Age", ylab="Frequency")

generate.health.score <- function(age, is.donor) {
  # Normalize age to a 0-1 scale
  age.normalized <- (age - age.min) / (age.max - age.min)
  
  # Create a non-linear age weighting effect (more pronounced for older ages)
  age.effect <- 1 - age.normalized^2 # Quadratic effect
  
  # Base score ranges
  # donor: [0.6, 1.0], recipient: [0.1, 0.8]
  base.score.min <- if(is.donor) 0.6 else 0.1
  base.score.max <- if(is.donor) 2.0 else 0.8
  
  # Generate base score
  base.score <- runif(1, base.score.min, base.score.max)
  
  # Combine base score with age effect
  health.score <- base.score * age.effect
  
  # Ensure health score is within [0, 1] range
  health.score <- pmax(0, pmin(1, health.score))
  
  return(health.score)
}

# Generate data for donors
generate.donor.data <- function(n) {
  ages <- generate.age(n, is.donor=TRUE)
  data.frame(
    id = paste0("D", 1:n),
    age = ages,
    blood.type = generate.blood.type(n),
    health.score = sapply(ages, generate.health.score, is.donor=TRUE),
    time = sample(1:24, n, replace=TRUE)  # Time available in hours
  )
}

# Generate data for recipients
generate.recipient.data <- function(n) {
  ages <- generate.age(n, is.donor=FALSE)
  data.frame(
    id = paste0("R", 1:n),
    age = ages,
    blood.type = generate.blood.type(n),
    health.score = sapply(ages, generate.health.score, is.donor=FALSE),
    time = sample(1:24, n, replace=TRUE)  # Time listed in hours
  )
}

# Generate data
data.d <- generate.donor.data(d)
data.r <- generate.recipient.data(r)

# Save data
save(data.d, data.r, file=paste0(cwd,"data.RData"))
