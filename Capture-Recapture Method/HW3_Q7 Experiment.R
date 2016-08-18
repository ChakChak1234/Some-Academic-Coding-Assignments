#  ------------------------------------------------------------------------
# 7. Can we get better estimates using larger sample sizes? Let's restrict
# n = n1 = n2. Write a function which computes the bias and variance of the
# Chapman estimator for varying sample sizes n. The inputs for this function
# should be: N, number of simulation runs per sample size, and n. The function
# should return in list form (a) a data frame with three columns:
# n, bias of estimator, and standard deviation of estimator for each sample size
# and (b) the true population size. Run this function using the following
# arguments: (a) N =100,000 for each sample size, (b) 1,000 simulation runs,
# and (c) n ranging from 100 to 5,000 (use seq(from=100, to=5000, by=50)).
# Include in your function an indicator of your progress every time you
# complete runs for 10 sample sizes. Based on your results, construct two plots:
# (a) bias versus n and (b) variance versus n (e.g., y-axis variable vs. x-axis
# variable). Indicate zero on both plots and connect the points with a line.
# Describe what you see.

# Population Size
N <- 100000
# Iterations per Sample Size
n.sim <- 1000
# Various Sample Sizes
samples.sim <- seq(from=100, to=5000, by=50)

cap.recap.partdeux <- function(N, n.sim, samples.sim){
n <- NA            # sample size
bias.of.est <- NA  # bias of estimator
sd.of.est <- NA    # standard deviation of estimators by sample
l.sim.chap <- NA   # temporary list of estimators to use for later
  # For Loop: Various Sample Sizes
  for(i in 1:length(samples.sim)){
    # For Loop: Iterations per Sample Size
    for (j in 1:n.sim){
      #i <- 1:1
      #j <- 1:2
      # Catch One
      sim.one <- sample(N, samples.sim, replace=T, prob=NULL)
      sim.one <- as.numeric(sim.one) # Convert to numeric
      # Catch Two
      sim.two <- sample(N, samples.sim, replace=T, prob=NULL)
      sim.two <- as.numeric(sim.two) # Convert to numeric
      # Find Common Elements
      sim.m.two <- intersect(sim.one, sim.two)
      # Amount of Common Elements
      sim.l.m.two <- length(sim.m.two)
      # Calculate Chapman Estimator
      sim.chap <- ((samples.sim[i]+1)*(samples.sim[i]+1)/(sim.l.m.two+1))-1
      l.sim.chap[j] <- list(sim.chap)
    } # End For Loop: Iterations per Sample Size
    # Calculate bias of estimator for each sample
    sum.b.est <- sum(unlist(l.sim.chap), na.rm=T)
    bias.est <- (sum.b.est/n.sim)-N
    bias.of.est[i] <- bias.est
    # Calculate standard deviation of estimator for each sample
    sd.est <- sd(unlist(l.sim.chap), na.rm = TRUE)
    sd.of.est[i] <- sd.est
    # Sample Size
    n[i] <- samples.sim[i]
    # Modulus operation
    if(i %% 10==0) {
      # Print on the screen some message
      cat(paste0("iteration: ", i, "\n"))
    }
  } # End For Loop: Various Sample Sizes
  # Return Three Columns and make Data Frame
  Three <- (data.frame(n, bias.of.est, sd.of.est))
  # List of Data Frame with True Population
  Output <- (list(Three, "POP"=N))
  return(Output)
} # End Function

Output <- cap.recap.partdeux(N=100000, n.sim=1000, samples.sim)
Test <- data.frame(Output)

# Plot bias of estimator on sample size
y.max <- max(c(max(hist(Test$"n", plot=FALSE)$density), 
dnorm(Test$"n", mean=Test$bias.of.est/length(Test$bias.of.est), sd=sd(Test[,2]))))
hist(Test$"n", freq=FALSE, las=TRUE, col="#45762450",
     main=paste("Number of Iterations: 1000"), 
     xlab="Sample Sizes", ylab="density", ylim=c(0, y.max))

# Plot variance of estimator on sample size
var.of.est <- (sd(Test[,3]))^2
y.max <- max(c(max(hist(Test$"n", plot=FALSE)$density), 
dnorm(Test$"n", mean=var.of.est/length(Test$sd.of.est), sd=sqrt(var.of.est))))
hist(Test$"n", freq=FALSE, las=TRUE, col="#45762450",
     main=paste("Number of Iterations: 1000"), 
     xlab="Sample Sizes", ylab="density", ylim=c(0, y.max))

