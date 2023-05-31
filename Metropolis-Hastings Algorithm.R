#################################
# Metropolis-Hastings Algorithm #
#################################


# target distribution
target <- function(x) {
  (1/2) * (1/sqrt(2*pi)) * exp(-(x-1)^2/2) + (1/2) * (1/sqrt(2*pi)) * exp(-(x-3)^2/2)
}

x <- rep(0, 1000)   # sample storage
x[1] <- 3  # staring value
accept_cnt <- 0   # the number of acceptance


for (i in 2:1000) {
  
  # current value
  currentx <- x[i-1]
  
  # proposed value
  proposedx <- currentx + rnorm(1, mean = 0, sd = 1)   # proposal distribution: normal distribution
  
  
  # acceptance probability
  p <- min(1, target(proposedx) / target(currentx))
  
  
  if (runif(1) < p) {
    x[i] <- proposedx   # accept
    accept_cnt = accept_cnt + 1
  } else {
    x[i] <- currentx   # reject
  }
}

par(mfrow = c(1, 2))

# trace plot
plot(x, type = "l",
     main = "Trace Plot",
     xlab = "Iteration number",
     ylab = "")

# histogram
hist(x,
     main = "Histogram of Samples",
     xlab = "",
     ylab = "Frequency")

# acceptance rate
acceptance_rate <- accept_cnt / 1000
acceptance_rate
