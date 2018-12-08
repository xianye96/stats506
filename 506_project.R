##R part

library(aod)
library(ggplot2)
library(dplyr)

##1) loading and training data
##  we select the "Population, perCapInc, PctPopUnderPov, pctUrban" as predictor variables. 
##  Delete the missing value.
##  I choose the first 1500 row as the fitting data set and the remaining 594 row as the testing data set. 
##  Divide the "ViolentCrimesPerPop" at point 0.5

communities.data <- read.csv("~/communities.data.txt", header=FALSE)
df3<-select(communities.data,c(6,17,26,34,128))
df3<-na.omit(df3)
a=rep(0,1994)
for(i in 1:1994){
  if(df3$V128[i]>0.5){
    a[i]=1
  }
}
df3<-cbind(df3,a)

## 2) Probit model
## The code below estimates a probit regression model using the glm (generalized linear model) function.


myprobit <- glm(a ~ V6 + V17 + V26 +V34, family = binomial(link = "probit"), data = df3)
summary(myprobit)

##  The deviance residuals are a measure of model fit. 
##  This part of output shows the distribution of the deviance residuals for individual cases used in the model.
##  The next part of the output shows the coefficients, their standard errors, 
##  the z-statistic (sometimes called a Wald z-statistic), and the associated p-values. 
##  Population, PctPopUnderPov, pctUrban are statistically significant while perCapInc doesn't have good performance. 
##  Below the table of coefficients are fit indices, including the null and deviance residuals and the AIC. 
##  Later we show an example of how you can use these values to help assess model fit.
##  In addition, we can obtain confidence intervals for the coefficient estimates that is based on the standard error and the normal assumption.

confint(myprobit)
##  We can test for an overall effect of "Population, PctPopUnderPov, pctUrban" using the wald.test function. 
wald.test(b = coef(myprobit), Sigma = vcov(myprobit), Terms = 2:5)
##  The chi-squared test statistic of 21.4 with three degrees of freedom is associated with a p-value of less than 0.001 indicating that the overall effect of rank is statistically significant.
##  Then we use testing data set and predicted probabilities to help us understand the model. 
df3[, c("p", "se")] <- predict(myprobit, df3, type = "response", se.fit = TRUE)[-3]
with(myprobit, null.deviance - deviance)
with(myprobit, df.null - df.residual)
with(myprobit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
##  We may also wish to see measures of how well our model fits. 
##  The chi-square of 305.8052 with 3 degrees of freedom and an associated p-value of less than 0.001 tells us that 
##  our model as a whole fits significantly better than an empty model.


## 3) Logistic model

mylogit <- glm(a ~ V6 + V17 + V26 +V34, data = df3, family = "binomial")
summary(mylogit)
confint.default(mylogit)
## From the z-statistic, we can see that just as the result of probit model, all the predictors are significant except for perCapInc
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 2:5)

with(mylogit, null.deviance - deviance)
with(mylogit, df.null-df.residual)
with(mylogit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
hl <- hoslem.test(mylogit$y, fitted(mylogit), g=5)










