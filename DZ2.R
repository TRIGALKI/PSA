df<-read.table("clipboard",h=TRUE,dec = ",",sep = "\t")
summary(m1<-lm(log(Y)~log(X2),data=df))
summary(m2<-lm(log(Y)~log(X1),data=df))
summary(m3<-lm(log(Y)~log(X1)+log(X2),data=df))
summary(m4<-lm(log(Y)~log(X2)+log(X3)+log(X4),data=df))
####2
summary(m5<-lm(Y~poly(X1,2,raw=TRUE),data=df))
summary(m6<-lm(Y~poly(X1,3,raw=TRUE),data=df))
summary(m7<-lm(Y~poly(X1,4,raw=TRUE),data=df))
summary(m8<-lm(Y~poly(X1,5,raw=TRUE),data=df))
summary(m9<-lm(Y~poly(X1,7,raw=TRUE),data=df))
summary(aov(m5))
summary(aov(m6))
summary(aov(m7))
summary(aov(m8))
summary(aov(m9))

x1lims=range(df$X1)
x1.grid=seq(from=x1lims[1], to=x1lims[2]) 
preds=predict(m8, newdata=list(X1 = x1.grid), se=TRUE)
se.bands=cbind(preds$fit+2*preds$se.fit, preds$fit-2*preds$se.fit) 
plot(Y~X1, data = df, xlim= x1lims, col='black') 
lines(x1.grid, preds$fit, lwd=2, col='orange') 
matlines(x1.grid, se.bands, lwd=2, col = 'blue', lty=3)
matlines(m8, color='red',data=df)

####3
Xthrd=c(3,5,7,9,9,10,11,13,17,19)
plot(density(Xthrd,kernel="gaussian"), lty=1);
plot(density(Xthrd,kernel="epanechnikov"), lty=2);

####4
install.packages("ISRL")
library("ISLR")
df4<-data.frame(Smarket, package="ISRL")
d<-data.frame(Lag1=df4$Lag1,Lag2=df4$Lag2,Lag3=df4$Lag3,Lag4=df4$Lag4,Lag5=df4$Lag5,volume=df4$Volume)
cor(d)
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume , data = Smarket, family=binomial)
summary(glm.fit)
coef(glm.fit)
df4$Pred2 = predict(glm.fit, type="response")
df4$PredCat2 = cut(df4$Pred2, c(0,0.5,1), include.lowest=TRUE, labels=c("Down","Up"))
sum(df4$PredCat2 == df4$Direction)/1250
####5
install.packages("pls")
library("pls")
pcr_model <- pcr(hdi~sub1+sub2+sub3+sub4, data = hdi)
summary(pcr_model)
hdi$Preds<-predict(pcr_model)


    