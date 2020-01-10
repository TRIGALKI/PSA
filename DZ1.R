#1===============================================>
install.packages("robustbase")
library("robustbase")
df<-data.frame(education, package="robustbase")
summary(df)
#2===============================================>
summary(m1<-lm(Y ~ X1,data = df ))
#3===============================================>
plot(df$Y~df$X1, xlab="x", ylab="y")
abline(m1)
#4=================================================>
summary(m2<-lm(Y ~ X1+X2+X3,data = df ))
#5=================================================>
anova(m2)
#6=================================================>
summary(m6<-lm(Y ~ X1+X2+X3+factor(Region),data = df ))
df <- within(df, Region <- as.factor(Region))
region=relevel(df$Region,2)
summary(m6<-lm(Y ~ X1+X2+X3+region,data = df ))
#8================================================>
plot(m6)
#9================================================>
library("lmtest")
bptest(m6, studentize = FALSE)
#10==================================================>
library("sandwich")
coeftest(m6, vcov = vcovHC(m6, "HC0"))
coeftest(m6, vcov = vcovHC(m6, "HC2"))
#7=================================================>
# 0.install package 
install.packages("ggplot2")
library("ggplot2")
# 1. Add predictions 
pred.int <- predict(m1, interval = "prediction") 
mydata <- cbind(education, pred.int) 
# 2. Regression line + confidence intervals 
library("ggplot2") 
p <- ggplot(mydata, aes(X1,Y)) + 
  geom_point() + 
  stat_smooth(method = lm) 
# 3. Add prediction intervals 
p+geom_line(aes(y = lwr), color = "orange", linetype = "solid")+ 
  geom_line(aes(y = upr), color = "orange", linetype = "solid")


#############################################################################
#lm.out <- lm(df$Y ~ df$X1)
#newx = seq(min(df$X1),max(df$X1),length.out = 50)
#conf_interval <- predict(lm.out, newdata=data.frame(x=newx), interval="prediction",
#                         level = 0.95)
#plot(df$X1, df$Y, xlab="x", ylab="y", main="Regression")
#abline(lm.out, col="lightblue")
#lines(newx, conf_interval[,2], col="blue", lty=2)
#lines(newx, conf_interval[,3], col="blue", lty=2)

