#1. This problem uses the UN11 data in the alr4 package.

install.packages('alr4')
library(alr4)


#(a) Plot fertility against ppgdp. Fit a linear model regressing fertility on ppgdp and add the fit on the plot. Comment on why this model is not good.

library(alr4)
data(UN11)
plot(UN11$ppgdp, UN11$fertility, xlab = 'ppgdp', ylab = 'fertility', main = 
       'ppgdp vs fertility', pch = 20, col = 'purple')
lm <- lm(UN11$fertility ~ UN11$ppgdp)
lm
abline(lm, col = 'red', lwd = 2)


#This is not a good model. It is not following the LINE properties when checking if the model is appropriate. From this gragh we can automatically fail the idea of being a good model by being able to tell that the population regression function is not linear but rather skewed right. In other words, majority of the data is clustered in one area.


#(b) Use a “residuals vs fit” plot to check if there is any non-constant variance or non-linearity problem. State the main problem and explain why in one or two sentences.

y_hat <- fitted(lm)
residual <- UN11$fertility - y_hat
plot(y_hat, residual, xlab = 'Fitted Values', ylab = 'Residuals', main = 'Residual vs Fit', 
     pch = 20, col = 'purple')
abline(h = 0, lty = 2)

#There is a non-linearity and a non- constant varience problem in this model. There are mainly several spreadout positive x values then a heavily negative cluster of x values and then a slight cluster of positive x values. Stated from before, we can tell that there is skewness issue, thus the model doesn't have normality. 


#(c) Use a normal Q-Q probability plot to check if the normality assumption is met. State the main problem and explain why in one or two sentences.**

hist(residual, xlab = 'Residual', main = 'Histogram of Residuals')

qqnorm(residual, main = 'Normal Q-Q Plot of Residuals', pch = 20, col = 'purple')
qqline(residual, col = 'red')


#The histogram or residuals shows that the residuals and error terms are skewed and thus not normally distributed. In the normal Q-Q plot, the relationship is far from being linear, which implies that the condition of the error terms are normally distributed is not met.


#(d) Shapiro-Wilk test is a test of normality of a numeric variable. The null hypothesis for this test is that the variable is normally distributed. Use the R function shapiro.test() to test if the residuals of the linear fit in part (a) is normally distributed. State the p-value of this test and your conclusion given $\alpha = 0.05$. Does the result support your conclusion in part (c)? (Use the code ?shapiro.test or help(shapiro.test) to understand how to use this function.)

shapiro.test(residual)

#P-value is 2.708e-08. The p-value is almost 0 which is less than ($\alpha = 0.05$.) Reject $H_0$. The residuals are not normally distrinuted. Thus the variable is not normally distributed which supports the conclusion in part (c). 


#2. This problem uses the teengamb data set in the faraway package. Fit a model with gamble as the response and the other variables as predictors.

install.packages('faraway')
library(faraway)


#(a) Predict the amount that men with average (given the data) status, income and verbal score would gamble along with an appropriate 95% confidence interval for the mean amount.

library(faraway)
data(teengamb)
fit <- lm(gamble ~ sex + status + income + verbal, data = teengamb)
fit
predictors <- data.frame(sex=0, status = mean(teengamb$status), income = mean(teengamb$income), 
                         verbal = mean(teengamb$verbal))
predictors
ci_95 <- predict(fit, predictors, se.fit = TRUE, interval = 'confidence', level = 0.95, 
type = 'response')
ci_95

#We are 95% confident that the mean predictors for the males are between 18.78277 and  37.70227 would gamble. 


#(b) Repeat the prediction for men with maximal values (for this data) of status, income and verbal score. Which confidence interval is wider and why is the result expected?**

library(faraway)
data(teengamb)
fit <- lm(gamble ~ sex + status + income + verbal, data = teengamb)
fit
predictors <- data.frame(sex=0, status = max(teengamb$status), income = max(teengamb$income), 
                         verbal = max(teengamb$verbal))
predictors
ci_95 <- predict(fit, predictors, se.fit = TRUE, interval = 'confidence', level = 0.95, 
type = 'response')
ci_95


#We are 95% confident that the max predictors for the males are between 42.23237 and 100.3835 would gamble. This confidence is wilder than the mean predictors because it makes sense that males with the best status, income and verbal would gamble more than the average. The max confidence interval takes the maximum in all predictor cases making it larger, where the mean confidence interval is much more condensed compared to this confidence interval by taking into account the average predictor values.


#(c) Fit a model with sqrt(gamble) as the response but with the same predictors. Now predict the response and give a 95% prediction interval for an individual in (a). Take care to give your answer in the original units of the response.

library(faraway)
data(teengamb)
fit <- lm(sqrt(gamble) ~ sex + status + income + verbal, data = teengamb)
fit
predictors <- data.frame(sex=0, status = mean(teengamb$status), income = mean(teengamb$income), 
                         verbal = mean(teengamb$verbal))
predictors
ci_95 <- predict(fit, predictors, interval = 'prediction')
ci_95
ci_95^2


#The 95% prediction interval from a is that the mean predictors for the males are between 0.06004216 and 69.6237 would gamble. 


#3. Using the sat data in the faraway package:

#(a) Fit a model with total sat score as the response and expend and takers as predictors. Test the hypothesis that $\beta_{expend} = \beta_{takers} = 0$. Do any of the two predictors have an effect on the response?

$H_0 : \beta_{expend} = \beta_{takers} = 0$ \
$H_1 : \beta_{expend} \ne 0$ and/or $\beta_{takers} \ne 0$

library(faraway)
data(sat)
fit <- lm(total ~ expend + takers, sat)
fit
summary(fit)

#Since the p-value for both expend and takers are less than $\alpha$, we reject the $H_0$. Therefore the null, $\beta_{expend} = \beta_{takers} = 0$, is not true. Thus, the two predictors have an affect on the response and they are both statistically significant.


#4. This problem uses the trade.union data in the SemiPar package.

install.packages('SemiPar')
library(SemiPar)

#(a) Plot the wage as a function of age using a different plotting symbol for the different union membership of the world.

library(SemiPar)
data(trade.union)
plot(trade.union$age,trade.union$wage, xlab = 'Age', ylab = 'Wage', main = 'Age vs Wage', 
     col = trade.union$union.member+1, pch = 1)
legend('topright', legend = paste('Union Member', 0:1), col = 1:2, pch = 1, bty = 'o')


#(b) Determine a transformation on the response wage to facilitate linear modeling with age and union membership as predictors.**

fit <- lm(wage ~ age + union.member, data = trade.union)
yhat <- fitted(fit)
residual <- trade.union$wage - yhat
plot(yhat, residual, xlab = 'Fitted Values', ylab = 'Residuals', main = 'Residual vs Fit', 
     pch = 1, col = 'blue')
abline(h = 0, lty = 2, lwd = 3)
hist(residual, xlab = 'Residual', main = 'Histogram of Residuals')
qqnorm(residual, main = 'Normal Q-Q Plot of Residuals', pch = 1, col = 'blue')
qqline(residual, col = 'red', lwd = 2)


#There is a non-linearity and an unequal varience issue in this problem. Therefore, the transformation needed is to take the log of the wage variable to resolve both issues.  

#Therefore, we make our transformation.

fit <- lm(log(wage) ~ age + union.member, data = trade.union)
fit
yhat <- fitted(fit)
residual <- log(trade.union$wage) - yhat
plot(yhat, residual, xlab = 'Fitted Values', ylab = 'Residuals', main = 'Residual vs Fit', 
     pch = 1, col = 'blue')
abline(h = 0, lty = 2, lwd = 3)
qqnorm(residual, main = 'Normal Q-Q Plot of Residuals', pch = 1, col = 'blue')
qqline(residual, col = 'red', lwd = 2)


#With the log transformation on wage, our linear model is fixed.

#(c) Fit a linear model regressing transformed wage on age and union membership. What is the relationship of age and union membership to wage?

fit <- lm(log(wage) ~ age + union.member, data = trade.union)
fit
summary(fit)


#There is a positive linear relationship between the the two predictors (age and union.member) and the response(log(wage)). The p-values are small enough for the predictors to have an effect on the response. 