# LINEAR MODELING

pitch = c(233,204,242,130,112,142)
sex = c(rep("female",3),rep("male",3))
my.df = data.frame(sex,pitch)
my.df

#make linear model
xmdl = lm(pitch ~ sex, my.df)
summary(xmdl)
#92.1% of what happened in dataset is explained by our model

#think about pitch as function of age
age = c(14,23,35,48,52,67)
pitch = c(252,244,240,233,212,204)
my.df = data.frame(age,pitch)
xmdl = lm(pitch ~ age, my.df)
summary(xmdl)

# Assumptions that must be met
 ## 1. Linearity 
plot(fitted(xmdl), residuals(xmdl))
 ## 2. Absence of collinearity 
# intelligence ratings ~ talking speed 
 ## 3. Homoskedasticity (variability of your data shuld be approximately equal across the range of your predicated values),
    #if this is the case, you can transform your data using a log-transform 
plot(rnorm(100), rnorm(100)) # shows examples of plots with roughly equal variances
 ## 4. Normality of residuals (least important)
hist(residuals(xmdl)) # should be relatively bell shaped
qqnorm(residuals(xmdl)) # data falls on straight line
 ## 5. Absense of influenctial data points
dfbeta(xmdl) # look for dfbeta values that are different by half of the absolute value of the slope
 ## 6. Independence (the most important)


# LINEAR MIXED EFFECT ANALYSES
library(lme4)
politeness= read.csv("http://www.bodowinter.com/tutorial/politeness_data.csv")

#Assumption: the same as above, BUT
## don't use dfbeta - doesn't work for mixed models 

#CITE
citation(lme4)
