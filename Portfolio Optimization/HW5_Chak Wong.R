# Q1 What is the start date and end date of this reduced data set? ---------
# Graph the federal funds interest rate as a time series (i.e., date on x-axis; 
# set type="l" in plot for connected points). Describe what you see in the plot
# and relate it briely to the recent financial crisis.

data.x <- read.table("asset_data.txt", header=TRUE, sep=",")
data.x$date <- as.Date(data.x$date, format="%Y-%m-%d")
data.complete <- data.x[complete.cases(data.x),]
rownames(data.complete) <- 1:nrow(data.complete)

plot(data.complete$fed.rate, type="l", las=TRUE, 
     xlab="Date (data collected weekly)", ylab="Frequency",  	
     main="Federal Funds Interest Rate")

# Answer: The start date is Janurary 8, 2003 and the end date is 
# October 29, 2014. We see a steady, gradual increase until the halfway, where
# there was a sharp drop, which remains steady and never increasing. This reflects
# our recent financial crisis in which the United States, and the entire world,
# collapsed into a recession.


# Q2 Now we will split the data into training and test sets. The training --
# will be used to compute our portfolio weights and our test set will be used 
# to evaluate our portfolio. Make two separate data frames; (a) the training 
# set should contain all observations before 2014 and (b) the test set should 
# contain all observations in 2014. How many observations are in each subset?

data.train <- data.complete[1:570,]
data.test <- data.complete[571:613,]
rownames(data.test) <- 1:nrow(data.test)

# Answer: The training set contains 570 observations, and the test set contains
# 43 observations.


# Q3 The federal funds interest rate is in percent form so convert it to --
# decimal (i.e., fractional) form. Calculate the Sharpe ratio for both
# SPY and TLT and store their respective total returns in the training set.
# Then, use each return to calculate a time series (make the y-axis have the
# same y-limites). Compare the two returns. What do you see?

# Convert fed rates to decimal
data.train$fed.rate <- data.train$fed.rate/100
data.test$fed.rate <- data.test$fed.rate/100

# Sharpe Ratio
data.train$spy.return <- NA
data.train$tlt.return <- NA
for(i in 2:length(data.train$close.spy)){
  x <- (data.train$close.spy[i]-data.train$close.spy[i-1])/data.train$close.spy[i-1]
  data.train$spy.return[i] <- x
} # End Loop
for(i in 2:length(data.train$close.tlt)){
  x <- (data.train$close.tlt[i]-data.train$close.tlt[i-1])/data.train$close.tlt[i-1]
  data.train$tlt.return[i] <- x
} # End Loop

# Time Series
plot(as.ts(data.train$spy.return))
plot(as.ts(data.train$tlt.return))

# Answer: There is a sharp dip around the year 2008-2009.


# Q4 The Sharpe ratio calculation assumes that returns are normally ----
# distributed. Construct two normal quantile plots, one for each asset. Is 
# this assumption satisfied? Justify your answer.

# QQ plots
qqnorm(data.train$spy.return)
qqnorm(data.train$tlt.return)

# Answer: The QQ-plots appear to be normal, thereby suggesting normal distributions.


# Q5 Compute the correlation between the S&P500 and long term treasury -----
# bond returns in training set and interpret it. Now, we will compute a 
# rolling-window correlation as follows: compute the correlation between the 
# two asset returns only using the first 24 days of data (i.e., day 2 to 25),
# next compute the correlation between the two asset returns for data from day 
# 3 through 26, then day 4 through 27, and so forth. Once you compute the 
# rolling-window correlations, make a time series plot of the rolling-window 
# correlation with each point plotted on the last day of the window. Add a
# horizontal, dotted, gray line at 0 to your plot. Is the correlation or 
# rolling-window correlation a better way to describe the relationship between 
# these two assets? Justify your answer.

# Correlation
cor(data.train$spy.return, data.train$tlt.return, use="complete.obs")

# Rolling Correlation
data.train$roll.cor <- NA
for(i in 2:547){
  x <- cor(data.train$spy.return[i:(i+23)],data.train$tlt.return[i:(i+23)])
  data.train$roll.cor[i] <- x
} #end "for" loop

# Time Series Roll Corr
plot(as.ts(data.train$roll.cor))
abline(h=0, col="gray60", cex=3)

# Answer: The correlation value is -0.34. The rolling window correlation is a 
# better way to describe the relationship between thesee two assets because it 
# allows us to view the steady, gradual ups and downs on the correlation, 
# instead of the strength and direction of the correlation.


# Q6 Compute the Sharpe ratios for each asset on the training set  --------
# Step 0. Let r.t be the return and y.t be the federal funds interest rate 
# for day t = 1...T.
# Step 1. Compute the excess returns, et, for each day in the data set.
#                         e.t = r.t - (y.t-1)/52  
# Excess returns are returns that you earn in excess to the risk free rate.
# Step 2. Convert the excess returns into an excess returns index, g.t:
#                 g1 = 100        g.t = g.t-1 * (1 + et.)
#
# Step 3. Compute the number of years of data, n, by taking the number of 
# weeks for which you have returns (i.e., number of observations in your 
# training set) and dividing by 52 (since there are 52 weeks in a year); 
# therefore the number of years of data can be a fractional amount.
# Step 4. Compute the compounded annual growth rate, CAGR:
#                 CAGR = [(g.t/g.1)^(1/n)] - 1
# Step 5. Compute the annualized volatility, v:
#                 p = sqrt(52)*SD[e.t]
# where SD[e.t] is the standard deviation of the excess returns.
# Step 6. Compute the Sharpe Ratio, SR, which is the ratio of the compounded 
# annual growth rate and the annualized volatility:
#                   SR = CAGR / v
# Which asset is a better investment? Justify your answer.

et = rt - (y.t-1)/52

# Step 1: Calculate excess returns for SPY and TLT
a.spy <- NULL
for(i in 1:length(data.train$fed.rate)){
  a <- data.train$fed.rate[i-1]/52
  a.spy[i] <- a
} # end For Loop
et.spy <- data.train$spy.return - a.spy
data.train$et.spy <- et.spy

et.tlt <- data.train$tlt.return - a.spy
data.train$et.tlt <- et.tlt

# Step 2: Calculate excess indices for SPY and TLT
gt.spy <- 100*(1+et.spy)
data.train$gt.spy <- gt.spy
gt.tlt <- 100*(1+et.tlt)
data.train$gt.tlt <- gt.tlt

# Step 3: Define number of years
n <- length(data.train$gt.tlt)/52

# Step 4: Compute Compounded Annual Growth Rate (CAGR):
data.train$CAGR.spy <- ((data.train$gt.spy/100)^(1/n))-1
data.train$CAGR.tlt <- ((data.train$gt.tlt/100)^(1/n))-1

# Step 5: Compute the annualized volatility, v:
data.train$v.spy <- sqrt(52)*sd(data.train$et.spy, na.rm=T)
data.train$v.tlt <- sqrt(52)*sd(data.train$et.tlt, na.rm=T)

# Step 6: Sharpe Ratio (SR = CAGR / v)
sr.spy <- data.train$CAGR.spy/data.train$v.spy
sr.tlt <- data.train$CAGR.tlt/data.train$v.tlt

# Time Series
plot(as.ts(sr.spy))
plot(as.ts(sr.tlt))

# Summary
summary(as.ts(sr.spy))
summary(as.ts(sr.tlt))

# Answer: S&P 500 appears to be the better investment for several reasons. First,
# its mean and median is higher compared to treasury bonds. Second, while S&P 500
# appears to have a wider range and larger negative value, these two critiques
# can be answered by the fact that the huge dip is mainly attributed to the 
# economic recession and it also has a larger maximum value, respectively.


# Q7 Write a function which takes the following inputs: (a) a vector of---
# weights (call this argument x), (b) a vector of returns for asset 1, 
# (c) a vector of returns for asset 2, and (d) a vector of the corresponding 
# weekly federal funds interest rates. The function will then do the following:
# for each weight value in your vector x, you will compute the Sharpe ratio for 
# the corresponding portfolio. To obtain the returns for the portfolio, use
# the following equation:
#           r.t,portfolio = xr.t,S&P500 + (1 - x)r.t,treasury
# That is, x proportion of the funds will be invested in the S&P500 ETF and 
# (1 - x) proportion of the funds into the treasury bond ETF. After you compute 
# the returns for the portfolio, apply the steps in question 6 to get the Sharpe
# ratio for that portfolio.The output of your function should be a vector of 
# Sharpe ratios corresponding to the input weight vector. Use the curve() 
# function to plot the Sharpe ratio for weights between 0 and 1. Do you see a 
# weight that produces the maximum Sharpe ratio?

r.port <- NA # vector of returns for portfolio
test <- function(x){
  for(i in 1:length(data.train$spy.return)){
    b <- x*(data.train$spy.return[i])+(1-x)*(data.train$tlt.return[i])
    r.port[i] <- b
  } # For Loop for portfolio returns
# Step 1: Calculate excess returns for portfolio
et.port <- r.port - a.spy

# Step 2: Calculate excess indices for portfolio
gt.port <- 100*(1+et.port)

# Step 3: Define number of years
# n <- length(data.train$gt.tlt)/52

# Step 4: Compute Compounded Annual Growth Rate (CAGR):
CAGR.port <- ((gt.port/100)^(1/n))-1

# Step 5: Compute the annualized volatility, v:
v.port <- sqrt(52)*sd(et.port, na.rm=T)

# Step 6: Sharpe Ratio (SR = CAGR / v)
sr.port <- CAGR.port/v.port
sr.port <- na.omit(sr.port)
print(sr.port)
} # end function

# Curve
h<-hist(sr.port, breaks=20, density=10, col="darkblue", xlab="Portfolio Returns", 
        main="Distribution of Weights") 
xfit<-seq(min(sr.port),max(sr.port),length=40) 
yfit<-dnorm(xfit,mean=mean(sr.port),sd=sd(sr.port)) 
yfit <- yfit*diff(h$mids[1:2])*length(sr.port) 
lines(xfit, yfit, col="black", lwd=2)

# Answer: The value x=0.005 is a potential weight to have maximum Sharpe Ratio.

# Q8 Using the training set, use optimize() to determine the optimum -----
# weight for each asset using the function you wrote in question 7; how much of 
# the funds should be allocated to each asset? What is the Sharpe ratio of the 
# overall portfolio? According to your analysis, is it better to invest in 
# S&P500 only, long term treasury bonds only, or your combined portfolio?
# Justify your answer.

# Optimize Function
optimize(f=test, interval=sr.port, lower=min(sr.port), upper=max(sr.port))

# Answer: About 55:45 ratio between S&P500 and Treasury Bonds should be invested.
# The combined portfolio is the better investment strategy because investing in
# different types of stocks with different behaviors allows more potential to
# gain profit while creating a safety-net in case of utmost losses.
