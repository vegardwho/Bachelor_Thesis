library("iml")
require("rpart")
library(csvread)
#import dataset created in corresponding python-file
train = read.csv("Iris_train.csv",header=TRUE)
train$X4= type.convert(train$X4)
test = read.csv("Iris_test.csv",header=TRUE)
test$X4= type.convert(test$X4)

head(test)
test=test[,2:6]
train=(train[,2:6])
#naming features
names(train) <- c('sepal.length', 'sepal.width', 'petal.length', 'petal.width','Species')
names(test) <- c('sepal.length', 'sepal.width', 'petal.length', 'petal.width','Species')

X=data.matrix(train[,1:4])
Xreg=matrix(1, 120, 1)
Xst=scale(X)

#linear regression by "hand"
Xreg=cbind(Xreg,Xst)
y=data.matrix(train[5])
yst=scale(y)
Xt=t(Xreg)
r=Xt%*%yst
print("r")
print(r)
C=t(Xreg)%*%Xreg
Cinv=solve(C)
print("beta")
beta=Cinv%*%r
print(beta)

#linear regression by machine
linreg=lm(Species ~., data=train)
summary(linreg)

lin1=lm(Species ~ sepal.length+sepal.width+petal.length, data=train)
print(linreg$xlevels)
#relative coefficients of determination used in shapley regression. 
R=summary(linreg)$r.squared
R1=summary(lm(Species ~ sepal.width+petal.length+petal.width, data=train))$r.squared
R2=summary(lm(Species ~ sepal.length+petal.length+petal.width, data=train))$r.squared
R3=summary(lm(Species ~ sepal.length+sepal.width+petal.width, data=train))$r.squared
R4=summary(lm(Species ~ sepal.length+sepal.width+petal.length, data=train))$r.squared


attributes(summary(linreg))
summary(linreg)$sigma
summary(linreg)$coefficients[1,1]
#relative coefficient 
print("R^2_ij")
print((R-R1)/(1-R1))
print((R-R2)/(1-R2))
print((R-R3)/(1-R3))
print((R-R4)/(1-R4))
print("U_j:")
print((R-R1))
print((R-R2))
print((R-R3))
print((R-R4))

print(summary(linreg)$coefficients[1])





#Shapley regression:
#Manual math for the R^2 values for combinations of features in the linear regression
R=summary(lm(Species ~ ., data=train))$r.squared
R1=summary(lm(Species ~ sepal.length, data=train))$r.squared
R2=summary(lm(Species ~ sepal.width, data=train))$r.squared
R3=summary(lm(Species ~ petal.length, data=train))$r.squared
R4=summary(lm(Species ~ petal.width, data=train))$r.squared
R12=summary(lm(Species ~ sepal.length+sepal.width, data=train))$r.squared
R13=summary(lm(Species ~ sepal.length+petal.length, data=train))$r.squared
R14=summary(lm(Species ~ sepal.length+petal.width, data=train))$r.squared
R23=summary(lm(Species ~ sepal.width+petal.length, data=train))$r.squared
R24=summary(lm(Species ~ sepal.width+petal.width, data=train))$r.squared
R34=summary(lm(Species ~ petal.length+petal.width, data=train))$r.squared
R123=summary(lm(Species ~ sepal.length+sepal.width+petal.length, data=train))$r.squared
R124=summary(lm(Species ~ sepal.length+sepal.width+petal.width, data=train))$r.squared
R234=summary(lm(Species ~ sepal.width+petal.length+petal.width, data=train))$r.squared
R134=summary(lm(Species ~ sepal.length+petal.length+petal.width, data=train))$r.squared

#Gamma/scaling for the features in the shapley equation
gamma0=factorial(0)*factorial(4-0-1)/factorial(4)
gamma1=factorial(1)*factorial(4-1-1)/factorial(4)
gamma2=factorial(2)*factorial(4-2-1)/factorial(4)
gamma3=factorial(3)*factorial(4-3-1)/factorial(4)

#shapley values for feature 1,2,3 and 4 respectivly.  
SV1=gamma0*R1+gamma1*(R12-R2+R13-R3+R14-R4)+gamma2*(R123-R23+R134-R34+R124-R24)+gamma3*(R-R234)
SV2=gamma0*R2+gamma1*(R12-R1+R23-R3+R24-R4)+gamma2*(R123-R13+R234-R34+R124-R14)+gamma3*(R-R134)
SV3=gamma0*R3+gamma1*(R13-R1+R23-R2+R34-R4)+gamma2*(R123-R12+R134-R14+R234-R24)+gamma3*(R-R124)
SV4=gamma0*R4+gamma1*(R24-R2+R34-R3+R14-R1)+gamma2*(R234-R23+R134-R13+R124-R12)+gamma3*(R-R123)
print('Shapley regression values for R^2')
print(c(SV1,SV2,SV3,SV4))#shapley values
print('Sum of shapley:')
print(sum(c(SV1,SV2,SV3,SV4)))#compare sum of shapley with r^2

#plot for gaussian clusters 
library(mclust)
set.seed(0)
#clPairs(train[,1:4], train[,5])
# Select 4 continuous variables and look for three distinct groups.
mod2 <- Mclust(train[, 1:4], 3)


#print(mod2$parameters$mean[,1])


print(predict.Mclust(mod2,test[,1:4])$classification-1)
png("gmm.png",height=1000, width=1000, pointsize=50,res = 50)
plot(mod2,what="classification")


dev.off()

plot(mod2,what="classification",cex.main=1, cex.lab=1, cex.axis=1,cex.sub=10)

