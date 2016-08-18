
# HW4_Chak Wong -----------------------------------------------------------
# Upload txt file
nobel.choco <- read.table("nobel_chocolate.txt", header=TRUE, sep= ",")
# Trim data to three columns
nobel.choco <- nobel.choco[,1:3]

# 1. According to Messerli, what is the variable ¡°number of Nobel laureates-
# per capita¡± supposed to measure? Do you think it is a reasonable measure? 
# Justify your answer.

# Answer: Messerli presumes variable "number of Nobel laureates per capita"
# measures a country's citizens' average cognitive ability. This measure is 
# unreasonable for a boundless amount of reasons, a few of which I will note: 
# 1) not every intelligent individual is a Nobel Laureate; 
# 2) Nobel Laureate's have different cognitive abilities; 
# 3) countries are unequal in economic, social, political, and technological 
# domains, all four of which facilities the propensity of a potential Nobel
# Laureates to be from or live in a country.


# 2. Are countries without Nobel prize recipients included in Messerli¡¯s study?
# If not, what types of bias(es) would that introduce?

# Answer: Messerli's study did not include countries without Nobel prize
# recipients. This creates a bias, on a statistical level, because a variable
# should contain more than one category to allow a variable to fluctuate, and
# on an applied, realistic level, because there's more than 23 countries in the
# world.


# 3. Are the number of Nobel laureates per capita and chocolate consumption
# per capita measured on the same temporal scale? If not, how could this 
# affect the analysis?

# Answer: The number of Nobel laureates per capita and chocolate consumption
# per capita are not measured on the same temporal scale. This affects the
# analysis for several reasons: 1) countries start at different timepoints
# and develop at unequal pace, which impacts the technology and education levels
# of the country, two aspects integral for a country's average education level
# and propensity of a Nobel Laureate; 2) chocolate are introduced at different
# time points and are received differently in each country


# 4. Construct a scatterplot of Nobel laureates vs. chocolate consumption.--
# Label Sweden on your plot (on the computer, not by hand). Compute the 
# correlation between these two variables and add it to the scatterplot. 
# How would you describe this relationship? Is correlation an appropriate 
# measure? Why or why not?

# Scatterplot
plot(nobel.choco$chocolate, nobel.choco$nobel_rate, 
     main= "Correlation between Nobel Laureates and Chocolate Consumption",
     xlab="Chocolate Consumption Rate", ylab="Number of Nobel Laureates")
text(nobel.choco[20,2], nobel.choco[20,3], labels = "Sweden", adj = NULL, 
     pos = NULL, offset = 0.5, vfont = NULL, cex = 1)
legend(x='bottomright', 
       legend=paste('Cor =',round(cor(nobel.choco[,2], nobel.choco[,3]),4)))

# Answer: The scatter plot implies a positive, curvelinear relationship between
# Number of Nobel Laureates and Chocolate Consumption Rate. Correlation is the
# correct statistical formula to use in line with a scatter plot; the former
# assesses how two variables are related, while the latter illustrates the 
# x,y intersection of each country.


# 5. Why is your correlation different from Messerli¡¯s? -------------------

# Answer: Sweden was removed from the initial correlation analysis.

# 6. Why does Messerli consider Sweden an outlier? How does he explain it?

# Answer: Messerli considers Sweden as an outlier because the country is ranked
# first in nobel laureates per capita and chocolate consumption per capita.
# Statistically, this skews the entire data set's distribution towards it.


# 7. Regress Nobel laureates against chocolate consumption (include Sweden):
# (a) What is the regression equation? (Include units of measurement.)
# (b) Interpret the slope.
# (c) Is the slope significant (conduct a hypothesis test and include your 
# regression output in your answer)? Test at the ¦Á = 0.05 level.
# (d) Add the regression line to your scatterplot.
# (e) Conduct a residual analysis to check the regression assumptions. Make 
# all plots within one figure.

# Linear Regression
regress <- lm(nobel.choco$nobel_rate~nobel.choco$chocolate)
# Regression Summary
summary(regress)
# Answer:(a) The regression equation is Y=a+bx, where a is the intercept and 
# b is the beta coefficient (i.e. covariance of Nobel laureates and chocolate
# consumption divided by the variance of Nobel laureates), and with the units of
# measure included, is Y=-3.4+2.496x
# Answer:(b) An increase of one Nobel laureate per capita is an increase of
# 2.496 chocolate consumption (kg/year per capita).
# Answer:(c) The slope is nearly infinitely significant (p<.000001)
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -12.888  -2.953  -0.213   1.992  19.279 
#
# Coefficients:
#               Estimate  Std. Error  t value    Pr(>|t|)
# (Intercept)    -3.400      2.699    -1.260        0.222  
# (Slope)         2.496      0.407     6.133     4.37e-06 ***
#   ---
#   Signif. codes:  0 ¡®***¡¯ 0.001 ¡®**¡¯ 0.01 ¡®*¡¯ 0.05 ¡®.¡¯ 0.1 ¡® ¡¯ 1
# 
# Residual standard error: 6.26 on 21 degrees of freedom
# Multiple R-squared:  0.6418,  Adjusted R-squared:  0.6247  
# F-statistic: 37.62 on 1 and 21 DF,  p-value: 4.374e-06
# Answer:(d)

# Scatterplot with regression line
plot(nobel.choco$chocolate, nobel.choco$nobel_rate,
     main= "Correlation between Nobel Laureates and Chocolate Consumption",
     xlab="Chocolate Consumption Rate", ylab="Number of Nobel Laureates")
text(nobel.choco[20,3], nobel.choco[20,2], labels = "Sweden", adj = NULL, 
     pos = NULL, offset = 0.5, vfont = NULL, cex = 1)
legend(x='bottomright', 
       legend=paste('Cor =',round(cor(nobel.choco[,2], nobel.choco[,3]),4)))
abline(lm(nobel.choco[,2]~nobel.choco[,3]))
# Answer:(e) I created a Q-Q plot (with normality line), density distribution 
# of the residuals, and Residual of chocolate consumption by number of Nobel
# Laureates.

# Residual Analysis of Regression
lm.resid <- resid(lm(nobel.choco[,2]~nobel.choco[,3]))
# Residual Plot
plot(nobel.choco$chocolate, lm.resid, ylab="Residuals", 
     xlab="Chocolate Consumption Rate", main="Number of Nobel Laureates") 
abline(0, 0)
plot(density(lm.resid))
qqnorm(lm.resid)
qqline(lm.resid)


# 8. Using your model, what is the number of Nobel laureates expected to be
# for Sweden? What is the residual? (Remember to include units of measurement.)

# Recall corresponding row for Sweden
nobel.choco[20,]
# Expected value for Sweden
Expected <- -3.4+2.496*(6.4)

# Answer: Since model is Y=-3.4+2.496x, we plug in Sweden's Nobel Laureate 
# number (x=6.4), and obtain the expected number of Nobel laureates for
# Sweden, which is 12.5744. The residual, which is the difference between
# the observed and expected values, is 19.2806. In other words, Sweden's actual
# number of Nobel Laureates (x=31.855) is 19 units above the predicted number of 
# Nobel Laureates.


# 9. Does increasing chocolate consumption cause an increase in the number
# of Nobel Laureates? Justify your answer.

# Answer: Based solely on statistical analysis: Yes; while considering real-
# world information: No. The purpose of statistics is to collect a sample of
# the population, study a specific phenomenon, and extrapolate the sample
# statistic to make an inference about the population parameter. However,
# the population, especially in a real world context, is fluidly dynamic and
# requires infintely more variabls than the two used in this study to make a
# notable, accurate, and realistic assumption.

