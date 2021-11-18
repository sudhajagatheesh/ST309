# week 8

rm(list = ls())
library(MASS)

attach(Boston)
str(Boston)
?Boston
?lm

lm1.Boston=lm(medv~lstat)
summary(lm1.Boston)

# check the component of this lm object
names(lm1.Boston)
lm1.Boston$rank

# confidence interval(95% confidence by default)
confint(lm1.Boston)

# To access the fitted values
lm1.Boston$fitted.values
predict(lm1.Boston)

# confidence interval for fitted value
predict(lm1.Boston, data.frame(lstat=c(5,10,15)), interval="confidence")

# confidence interval for predicted value
predict(lm1.Boston, data.frame(lstat=c(5,10,15)), interval="prediction")

# draw a fitted line across the data
plot(lstat, medv); abline(lm1.Boston,lty=2,col=2)

# draw diagnostics plots
par(mfrow=c(2,2))
plot(lm1.Boston)
# residual plots are clearly not patternless!

# H matrix
X <- cbind(rep(1,nrow(Boston)),lstat)
Y <- as.matrix(medv)
beta <- solve(t(X) %*% X) %*% t(X) %*% Y
H <- X %*% solve(t(X) %*% X) %*% t(X)
diag(H)
sum(diag(H)) # which is p + 1 = 2, here we have only one predictor so p=1 

# standardized residual
res <- lm1.Boston$residuals
s_res <- (res - mean(res))/sd(res)
#leverage plot
plot(diag(H),s_res,main='Residual vs Leverage',
     xlab = 'Leverage',ylab = 'Standardized Residuals')

# based on the leverage plot
# 215 413 and 375 are bad leverage point, we need to remove it from the analysis
Boston1=Boston[-c(215,413,375),] # remove 3 bad leverage points
dim(Boston1)

lm11=lm(medv~lstat, data =Boston1) # Not lm(medv~lstat) now!
summary(lm11)
plot(lm11)

# try including all the predictors
lm2.Boston=lm(medv~., Boston) # include all the predictors
summary(lm2.Boston)

# remove the one with largest p values, age
lm3.Boston = lm(medv~.-age, Boston)
summary(lm3.Boston)

#Similarly, remove indus
lm4.Boston = lm(medv~.-age-indus, Boston)
summary(lm4.Boston)


# ---- step wise regression with 'step' ---- #
lm0.Boston = lm(medv~1)
summary(lm0.Boston)

# The selected model will be between ls0.Boston and ls2.Boston.
step.Boston=step(lm0.Boston, scope=list(upper=lm2.Boston))
summary(step.Boston) # it selects the the same predictors as in lm4.Boston
detach(Boston)

# --- Regression with qualitative Predictors --- #
data(Carseats,package = 'ISLR')
str(Carseats)

# interaction terms Income:Advertising and Price:Age in the model
# Income x Advertising and Price x Age
lm.Sales=lm(Sales~.+Income:Advertising+Price:Age, data=Carseats)
summary(lm.Sales)


attach(Carseats)
# encoding of dummy variables
contrasts(ShelveLoc)

# --- Cross validation for selecting tree models ---- #
library(tree)
High=as.factor(ifelse(Sales<=8, "No", "Yes"))
Carseats2=data.frame(Carseats, High)
tree.carseats=tree(High~.-Sales, Carseats2)
summary(tree.carseats)

# apply CV method to determine the tree size
?cv.tree
?prune.misclass
# by default, it runs a 10-folds CV
cv.carseats=cv.tree(tree.carseats, FUN=prune.misclass)

rbind(cv.carseats$size,cv.carseats$dev)

par(mfrow=c(1,1))
plot(cv.carseats$size,cv.carseats$dev,type ='b')

prune.carseats=prune.misclass(tree.carseats, best=17)
plot(prune.carseats)
text(prune.carseats, pretty=0)




