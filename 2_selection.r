# 2. Selection
# Selection is based on the compatible blood types, health score difference, age difference, and time listed 

rm(list=ls())
load.path <- file.path(getwd(), "1_data_initial.RData")
load(load.path)

# 1. Compatibility based on blood type (matrix)
comp.matrix <- matrix(FALSE, nrow = nrow(data.r), ncol = nrow(data.d))
rownames(comp.matrix) <- data.r$id
colnames(comp.matrix) <- data.d$id

# initial filtering based on blood compatible type
is.same.blood.type <- function(donor, recipient){
    compatible <- list(
        "O" = c("A", "B", "AB", "O"),
        "A" = c("A", "AB"),
        "B" = c("B", "AB"),
        "AB" = c("AB")
    )
    return(recipient %in% compatible[[donor]])
}

# filter based on blood type
for (i in 1:nrow(data.r)) {
    for (j in 1:nrow(data.d)) {
        comp.matrix[i, j] <- is.same.blood.type(data.d$blood.type[j], data.r$blood.type[i])
    }
}
print(comp.matrix)

# 2. Selection funciton
selection.score <- function(donor, recipient, weights){
    # normalize age
    age.diff <- abs(donor$age - recipient$age) / 80

    # health score difference
    health.diff <- max(0, donor$health.score - recipient$health.score)

    # time factor (24 hours for now)
    time.factor <- min(1, recipient$time.listed / 24) 
    
    # calculate score based on 3 age, health, time factors
    score <- weights$age * age.diff + 
            weights$health * health.diff + 
            weights$time * time.factor

    # between 0,1
    return(pmax(0, pmin(1, score)))
}

weights <- list(age = 0.4, health = 0.3, time = 0.3)
selection.matrix <- selection.score(data.d, data.r, weights)
selection.matrix[!comp.matrix] <- -1

# 3. Selection matrix
selection.matrix <- matrix(0, nrow = nrow(data.r), ncol = nrow(data.d))
rownames(selection.matrix) <- data.r$id
colnames(selection.matrix) <- data.d$id

for (i in 1:nrow(data.r)) {
    for (j in 1:nrow(data.d)) {
        if (comp.matrix[i, j]) {
            selection.matrix[i, j] <- selection.score(data.d[j,], data.r[i,], weights)
        } else {
            selection.matrix[i, j] <- -1
        }
    }
}
print(selection.matrix)

select.recipient <- function(selection.matrix){
    n <- nrow(selection.matrix)
    m <- ncol(selection.matrix)

    # find the highest score for each donor
    data <- data.frame(donor = character(n), recipient = character(n), score = numeric(n))
    for (i in 1:n) {
        max.score <- max(selection.matrix[i,])
        if (max.score > 0) {
            j <- which(selection.matrix[i,] == max.score)
            data[i,] <- c(data.d$id[i], data.r$id[j], max.score)
        }
    }
    return(data)
}

selected.df <- select.recipient(selection.matrix)

save(selection.matrix, comp.matrix, selected.df, file = "2_data_selection.RData")
