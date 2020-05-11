#1. This problem uses the wblake data set in the alr4 package. This data set includes samples of small mouth bass collected in West Bearskin Lake, Minnesota, in 1991. Interest is in predicting length with age. Finish this problem without using lm().

install.packages('alr4')
library(alr4)

#(a) Compute the regression of length on age, and report the estimates, their standard errors, the value of the coefficient of determination, and the estimate of variance. Write a sentence or two that summarizes the results of theses computations. 

library(alr4)
data(wblake)
avg1 <- mean(wblake$Age)
avg1
avg2 <- mean(wblake$Length)
avg2
Sxy <- sum((wblake$Age-avg1)*(wblake$Length-avg2))
Sxy
Sxx <-sum((wblake$Age-avg1)^2)
Sxx
slope <- Sxy / Sxx
slope
intercept <- avg2 - (avg1*slope)
intercept
y_hat <- intercept + slope*wblake$Age
n <- length(wblake$Length)
SSE <- sum((wblake$Length-y_hat)^2)
var <- SSE/(n-2)
var
se_age <- sqrt(var) / (sqrt(Sxx))
se_age
se_length <- sqrt(var) * (sqrt(1/n + (avg1)^2/Sxx))
se_length
SSTO <- sum((wblake$Length-avg2)^2)
R_2 <- 1- (SSE / SSTO)
R_2


#The regression of length on age is $\hat{Y} = 65.52716 + 30.32389x$. The estimates of this regression is the slope and intercept. The estimate on the slope is ($b_1 = 30.32389$) and the estimate on the intercept is ($b_0 = 65.52716$). The standard error for age is 0.6877291 and standard error for length is 3.197388. The coefficient of determination is 0.816477 implying the proportion of the variation to length. The estimate of the varience is 820.5847.


#(b) Obtain a 99% confidence interval for $\beta_1$ from the data. Interpret this interval in the context of the data.

t_pct <- qt(p = .995, df = n - 2)
ci_b1_99 <- slope + c(-1, 1)*t_pct*se_age
ci_b1_99


#We are 99% confident that the small mouth bass collected grows or has a length between 28.54465 and 32.10313 for every unit increase in age.


#(c) Obtain a prediction and a 99% prediction interval for a small mouth bass at age 1. Interpret this interval in the context of the data.
  
t_pct <- qt(p = .995, df = n - 2)
t_pct
x = 1
n <- length(wblake$Length)
y_hat1 <- intercept + slope * x
y_hat1
new_se <- sqrt(var) * (sqrt(1 + 1/n + ((x-avg1)^2)/Sxx))
new_se
ci_pred_99 <- y_hat1 + c(-1, 1)*t_pct*new_se
ci_pred_99


#We are 99% confident that the length of the small mouth bass at age 1 will be between 21.43775 and 170.26436. 


#2. This problem uses the data set Heights data set in the alr4 package. Interest is in predicting dheight by mheight.

#(a) Compute the regression of dheight on mheight, and report the estimates, their standard errors, the value of the coefficient of determination, and the estimate of the variance.
  
library(alr4)
data(Heights)
fit <- lm(Heights$dheight ~ Heights$mheight)
fit
summary(fit)
summary(fit)$sigma^2


#The regression of length on age is $\hat{Y} = 29.9174 + 0.5417x$. The estimates of this regression is the slope and intercept. The estimate on the slope is ($b_1 = 0.5417$) and the estimate on the intercept is ($b_0 = 29.9174$). The standard error for mheight is 0.02596 and standard error for dheight is 1.62247. The coefficient of determination is 0.2408 implying the proportion of the variation to dheight. The estimate of the varience is 5.136167.


#(b) For this problem, give an interpretation for $\beta_0$ and $\beta_1$.
  
# $\beta_0$ and $\beta_1$ are involving the population intercept and population slope in a simple regression model respectively. In this case for $\beta_0$, the expected mean value of dheight is 29.9174 when mheight is zero and for $\beta_1$ dheight increases by 0.5417 for every one unit of mheight. 


#(c) Obtain a prediction and a 99% prediction interval for a daughter whose mother is 64 inches tall.**
  
fit <- lm(dheight ~ mheight, data = Heights) 
fit  
summary(fit) 
new <- data.frame(mheight = 64)
ans <- predict(fit,new, se.fit = TRUE, interval = 'prediction', level = 0.99, type = 'response')
ans


#We are 99% confident that the daughter's height will be between 58.74045 and 70.43805 inches when the mother's height is 64 inches. 

#3(d) Simulate a data set with n = 100 observation units such that $Y_i = 1 + 2x_i + \epsilon_i$, i = 1, . . . , n. $\epsilon_i$ follows the standard normal distribution, i.e., a normal distribution with zero mean and unit variance. Use the result in (c) to compute $b_0$ and $b_1$. Show that they are the same as the estimates by lm(). Start with generating x as
         
         _n = 100_
       _x = seq(0, 1, length = n)_
         
       #(Hint: check the help page of rnorm() about how to simulate normally distributed random variables. Use solve() to get an inverse matrix and use t() to get a transpose matrix.)
         
        
       n = 100 
       x <- seq(0, 1, length = n)
       y <- 1 + 2*x + rnorm(n)
       X <- cbind(1,x)
       Y <- matrix(y)
       solve <- solve(t(X)%*%X)%*%t(X)%*%Y 
       solve
       fit <- lm(y ~ x)
       fit
      
       
       #The estimates are the same as the lm() function. They are always subject to change with this specific code due to the fact that there are 100 random values that will always be generated.