# Population size
N.seven <- 100000
# Total simulation 
n.for.sim <- rep(seq(from=100, to=5000, by=50), length.out=1000)
n.for.sim <- sort(n.for.sim)
# Calculate number of simulation per n sample if iteration=1000
sim.per.n <- as.data.frame(table(n.for.sim))
colnames(sim.per.n) <- c("Sample Size", "Amount")
sim.per.n$"Sample Size" <- seq(from=100, to=5000, by=50)

cap.recap.part.deux <- function(N.seven, sim.per.n, n.for.sim){
sim.chap <- NULL
n <- NULL
bias.estimator <- NULL
sd.estimator <- NULL
  # For Loop to calculate Chapman and store it and its sample size into vectors
  for(i in 1:length(n.for.sim){
    # First catch
    c.one <- sample(N.seven, n.for.sim[i], replace=T, prob=NULL)
    c.one <- as.numeric(c.one)
    # Second catch
    c.two <- sample(N.seven, n.for.sim[i], replace=T, prob=NULL)
    c.two <- as.numeric(c.two)
    # Common elements between first and second catches
    c.m.two <- intersect(c.one, c.two)
    # Numer of common elements
    c.m.two <- length(c.m.two)
    # Calculate Chapman
    sim.chap <- ((n.for.sim[i]+1)*(n.for.sim[i]+1)/(c.m.two+1))-1
    sim.chap[i] <- sim.chap
    n[i] <- n.for.sim[i]
    bias.estimator[i] <- sim.chap - 100000
  } # End For Loop for Number of Simulation per sample
  # For Loop to calculate std deviation of estimator for each sample size
  for(i in 1:sim.chap[110]){
    j <- seq(sim.chap[1], sim.chap[110], by=11)
    sd.estimator[i] <- sd(j)
    
    j <- seq(sim.chap[111], sim.chap[990], by=10)
    sd.estimator[i] <- sd(j)
    
  } # End For Loop to calculate std of estimator for each sample size
} # End Function


# Population size
N.seven <- 100000
# Total simulation 
n.for.sim <- rep(seq(from=100, to=5000, by=50), length.out=1000)
n.for.sim <- sort(n.for.sim)
# Calculate number of simulation per n sample if iteration=1000
sim.per.n <- as.data.frame(table(n.for.sim))
colnames(sim.per.n) <- c("Sample Size", "Amount")
sim.per.n$"Sample Size" <- seq(from=100, to=5000, by=50)

loopy.loop <- function(N.seven, sim.per.n, n.for.sim){
  # For Loop for varying sample sizes for simulation
  for(i in 1:length(n.for.sim)){
    # For Loop for how many iterations per sample size
    for(j in n:length(sim.per.n[,2])){
      c.one <- sample(N.seven, n.for.sim[i])
    } # End For Loop for how many iteration per sample size
  } # End For Loop for varying sample sizes for simulation
} # End Function


# Total Population
N <- 100000
# Number of simulation per sample size
sim.per.n <- rep(n, length.out=1000)
# n: sample size
sim.samples <- seq(from=100, to=5000, by=50)

cap.recap.part.deux <- function(N, sim.per.n, sim.samples){
n <- NULL          # sample size
biased.est <- NULL # biased estimator
std.est <- NULL    # standard deviation of estimator per sample
b <- NULL          # temp list of estimators used to calculate sd. per sample
  # FOR LOOP sim.per.n
  for(i in 1:length(sim.per.n)){
    # Catch One
    sim.one <- sample(N, sim.per.n, replace=T, prob=NULL)
    sim.one <- as.numeric(sim.one)
    # Catch Two
    sim.two <- sample(N, sim.per.n, replace=T, prob=NULL)
    sim.two <- as.numeric(sim.two)
    # M2
    sim.m.two <- intersect(sim.one, sim.two)
    # if m2 == 0
    if(length(sim.m.two) == 0){
      b[i] <- as.character(0)
    }else{
      sim.m.two.values <- sim.m.two
      sim.m.two.values <- toString(sim.m.two.values)
      b[i] <- sim.m.two.values
    }# end if/else
    # Number of common elements
    sim.m.two <- length(sim.m.two)
    # Calculate Chapman
    sim.chap <- ((sim.per.n+1)*(sim.per.n+1)/(sim.m.two+1))-1
    # Bias of estimator
    bias.est <- sim.chap-N
    biased.est[i] <- bias.est
    n[i] <- sim.per.n[i]
  } # end FOR LOOP
  # Convert m2 vector into numeric
  b <- data.frame(b)
  s <- strsplit(as.character(b[,1]), ", ")
  c <- sapply(s, length)
  # Standard Deviation of Estimator by Sample
  std.est <- unsplit(lapply(split(c,c),function(x) rep(sd(x),length(x))),c)
  # Data frame of n, bias estimator, std estimator
  three.columns <- data.frame(n, biased.est, std.est)
  # Return output of m2 & N^LP data frame and N
  return(list(three.columns, N))
} # end function
Output <- cap.recap.part.deux(N, sim.per.n, sim.samples)
test <- data.frame(Output)


n <- rep(seq(100, 5000, by=50), length.out=1000)
b <- NULL

std.est <- unsplit(lapply(split(b,b),function(x) rep(sd(x),length(x))),b)







# 1000 iteration per sample size ------------------------------------------

# Population Size
N <- 100000
# Iterations per Sample Size
n.sim <- 1000
# Various Sample Sizes
samples.sim <- seq(from=100, to=5000, by=50)

cap.recap.partdeux <- function(N, n.sim, samples.sim){
n <- NULL            # sample size
bias.of.est <- NULL  # bias of estimator
sd.of.est <- NULL    # standard deviation of estimators by sample
  # For Loop: Various Sample Sizes
  for(i in 1:length(samples.sim)){
    # For Loop: Iterations per Sample Size
    for (j in i:length(n.sim)){
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
      sim.chap <- ((samples.sim+1)*(samples.sim+1)/(sim.l.m.two+1))-1
      # store Sample Size
      n[j] <- samples.sim
    } # End For Loop: Iterations per Sample Size
  } # For Loop: Various Sample Sizes
} # End Function
Results <- cap.recap.partdeux(N, n.sim, samples.sim)
Results <- data.frame(Results)

# Population Size
N <- 100000
# Iterations per Sample Size
n.sim <- 1000
# Various Sample Sizes
samples.sim <- seq(from=100, to=5000, by=50)

cap.recap.part.deux <- function(N, n.sim, samples.sim){
n <- NULL           # Sample Size
bias.of.est <- NULL # Bias of Estimator
sd.of.est <- NULL   # Standard Deviation of Estimator

# For Loop: Sample Sizes
for(i in 1:length(samples.sim)){
  # For Loop: Iterations
  for(j in 1:n.sim){
    # First Catch
    sim.one <- sample(N, n.sim, replace=T, prob=NULL)
    sim.one <- as.numeric(sim.one)
    # Second Catch
    sim.two <- sample(N, n.sim, replace=T, prob=NULL)
    sim.two <- as.numeric(sim.two)
    # Common Elements
    sim.m.two <- intersect(sim.one, sim.two)
    # Amount of Common Elements
    l.m.two <- length(sim.m.two)
    # Calculate Chapman
    sim.chap <- ((samples.sim+1)*(samples.sim+1)/(l.m.two+1))-1
  } # End For Loop: Iterations
} # End For Loop: Sample Sizes 
  
} # End Function

# Population Size
N <- 100000
# Iterations per Sample Size
n.sim <- 1000
# Various Sample Sizes
samples.sim <- seq(from=100, to=5000, by=50)

cap.recap.part.deux <- function(N, n.sim, samples.sim){
l.sim.chap <- NULL  # Store Chapman Estimator per Iteration
n <- NULL           # Sample Size
bias.est <- NULL # Bias of Estimator
sd.est <- NULL   # Standard Deviation of Estimator
  for(i in 1:n.sim){
    # Catch One
    sim.one <- sample(N, samples.sim[1], replace=T, prob=NULL)
    sim.one <- as.numeric(sim.one)
    # Catch Two
    sim.two <- sample(N, samples.sim[1], replace=T, prob=NULL)
    sim.two <- as.numeric(sim.two)
    # Common Elements
    sim.m.two <- intersect(sim.one, sim.two)
    # Amount of Common Elements
    l.m.two <- length(sim.m.two)
    # Chapman Estimator
    sim.chap <- (((samples.sim[1]+1)*(samples.sim[1]+1))/(l.m.two+1))-1
    l.sim.chap[i] <- sim.chap
  } # End For Loop: 1000 Iterations
  Output <- return(l.sim.chap)
  # Bias of Estimator
  bias.of.est <- ((sum(Output[1:1000,]))/(n.sim))-N
  bias.est <- bias.of.est
  # Standard Deviation of Estimator
  sd.of.est <- sd(Output[1:1000,], na.rm=T)
  sd.est <- sd.of.est
  # Sample Size
  n <- 100
  # Data Frame of Three Columns: n, bias.of.est, sd.of.est
  Three <- data.frame(n, bias.est, sd.est)
  # Return List
  Result <- return(list(Three, N))
} # End Function

Test <- cap.recap.part.deux(N=100000, n.sim=1000, samples.sim[1])
Test <- data.frame(Test)



cap.recap.part.deux <- function(N, sim.per.n, sim.samples){
  n <- NULL          # sample size
  biased.est <- NULL # biased estimator
  std.est <- NULL    # standard deviation of estimator per sample
  b <- NULL          # temp list of estimators used to calculate sd. per sample
  # FOR LOOP sim.per.n
  for(i in 1:length(sim.per.n)){
    # Catch One
    sim.one <- sample(N, sim.samples, replace=T, prob=NULL)
    sim.one <- as.numeric(sim.one)
    # Catch Two
    sim.two <- sample(N, sim.samples, replace=T, prob=NULL)
    sim.two <- as.numeric(sim.two)
    # M2
    sim.m.two <- intersect(sim.one, sim.two)
    # if m2 == 0
    if(length(sim.m.two) == 0){
      b[i] <- as.character(0)
    }else{
      sim.m.two.values <- sim.m.two
      sim.m.two.values <- toString(sim.m.two.values)
      b[i] <- sim.m.two.values
    }# end if/else
    # Number of common elements
    sim.m.two <- length(sim.m.two)
    # Calculate Chapman
    sim.chap <- ((sim.samples+1)*(sim.samples+1)/(sim.m.two+1))-1
    # Bias of estimator
    bias.est <- sim.chap-N
    biased.est[i] <- bias.est
    n[i] <- sim.per.n[i]
  } # end FOR LOOP
  # Convert m2 vector into numeric
  b <- data.frame(b)
  s <- strsplit(as.character(b[,1]), ", ")
  c <- sapply(s, length)
  # Standard Deviation of Estimator by Sample
  std.est <- unsplit(lapply(split(c,c),function(x) rep(sd(x),length(x))),c)
  # Data frame of n, bias estimator, std estimator
  three.columns <- data.frame(n, biased.est, std.est)
  # Return output of m2 & N^LP data frame and N
  Output <- return(list(three.columns, N))
} # end function
Output <- cap.recap.part.deux(N, sim.per.n, sim.samples)
test <- data.frame(Output)