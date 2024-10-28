rm(list = ls())

# Function to simulate doctor evaluation
sim.doctor.eval <- function(donor, recipient){
    # age difference (normalized)
    age.diff <- abs(donor$age - recipient$age) / 80 # max age difference is 80

    # health score difference (considering donor should be healthier than recipient)
    health.diff <- max(0, donor$health.score - recipient$health.score)

    # time factor (preference for longer-waiting recipients)
    # recipient$time in 24 hour period for a given day
    time.factor <- min(1, recipient$time.listed / 24) # IMPLEMENT TOTAL ELAPSED TIME DIVISION instead of /24

    approval.prob <- (
        0.4 * (1 - age.diff) +  # Age difference contributes 40%
        0.3 * donor$health.score +  # Donor health contributes 30%
        0.25 * health.diff +  # Health difference contributes 25%
        0.05 * time.factor  # Time factor contributes 5%
    )

    # Add some randomness to represent individual doctor's judgment 
    approval.prob <- pmax(0, pmin(1, approval.prob + rnorm(1, mean = 0, sd = 0.1)))

    return(runif(1) < approval.prob)
}

num.doctors <- 5 
doctor.evals <- array(FALSE, dim = c(nrow(data.r), nrow(data.d), num.doctors))

# For each donor, evaluate only compatible recipients
for (j in 1:nrow(data.d)) { # for each donor
    comp.recipients <- which(comp.matrix[, j]) # get compatible recipients
    for (i in comp.recipients) { 
        for (k in 1:num.doctors) { 
            doctor.evals[i, j, k] <- sim.doctor.eval(data.d[j,], data.r[i,]) # evaluate each recipient by each doctor for the donor
        }
    }
}

# Function to get doctor consensus for a specific donor
get.consensus <- function(donor.idx) {
    comp.recipients <- which(comp.matrix[, donor.idx])
    consensus <- rep(FALSE, length(comp.recipients))
    for (i in seq_along(comp.recipients)) {
        recipient_index <- comp.recipients[i]
        approvals <- sum(doctor.evals[recipient_index, donor.idx, ])
        consensus[i] <- approvals > num.doctors/2
    }
    return(data.frame(
        recipient.id = data.r$id[comp.recipients],
        consensus = consensus,
        approvals = apply(doctor.evals[comp.recipients, donor.idx, ], 1, sum)
    ))
}


# Print results for each donor\
donor.sample <- sample(1:nrow(data.d), 1, replace=FALSE)
print(data.d[donor.sample,])
get.consensus(donor.sample)
