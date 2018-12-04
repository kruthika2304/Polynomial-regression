set.seed(0)
cars1 <- read.csv("C:/Users/hp/Downloads/cars.csv")
rand = sample(1:nrow(cars1),20)
train = cars1[rand, ]
test = cars1[-rand, ]
summary(test)
summary(cars1)


m1 <- lm(MPG~Weight, train)
m1

#PLOTTING THE MODEL OVER THE DATA
#jpeg('rplot.jpg', width = 350, height = 350)
plot(train$Weight,train$MPG, pch=19, cex=0.5)
lines(sort(train$Weight), fitted(m1)[order(train$Weight)], col='red', type='l') 
#dev.off()
#TRAIN AND TEST ACCURACY
t1=sum(m1$residuals^2)
t1
pred = predict(m1, newdata=test)
e1=sum((pred-test$MPG)^2)
Rmse1=sqrt(mean(e1))
Rmse1



m2 <- lm(MPG~Weight + I(Weight^2), train)
m2

#PLOTTING THE MODEL OVER THE DATA
plot(train$Weight,train$MPG, pch=19, cex=0.5)
lines(sort(train$Weight), fitted(m2)[order(train$Weight)], col='blue', type='l') 

#TRAIN AND TEST ACCURACY
t2=sum(m2$residuals^2)
t2
pred = predict(m2, newdata=test)
e2=sum((pred-test$MPG)^2)
Rmse2=sqrt(mean(e2))
Rmse2

m7 <- lm(MPG~Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)+ I(Weight^5) + I(Weight^6) 
         + I(Weight^6) + I(Weight^7), train)

m7







#PLOTTING THE MODEL OVER THE DATA
#TRAIN AND TEST ACCURACY
t7=sum(m2$residuals^2)
t7
pred = predict(m7, newdata=test)
e7=sum((pred-test$MPG)^2)
Rmse7=sqrt(mean(e7))
Rmse7



m8 <- lm(MPG~Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) +
           I(Weight^5) + I(Weight^6) + I(Weight^7) +
           I(Weight^8) , train)
m8

t8=sum(m8$residuals^2)
t8
pred = predict(m8, newdata=test)
e8=sum((pred-test$MPG)^2)
Rmse8=sqrt(mean(e8))
Rmse8




m9 <- lm(MPG ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) +
           I(Weight^5) + I(Weight^6) + I(Weight^7) +
           I(Weight^8) + I(Weight^9) , train)

m9

t9=sum(m9$residuals^2)
t9
pred = predict(m9, newdata=test)
e9=sum((pred-test$MPG)^2)
Rmse9=sqrt(mean(e9))
Rmse9

m10 <- lm(MPG ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) +
            I(Weight^5) + I(Weight^6) + I(Weight^7) +
            I(Weight^8) + I(Weight^9)+ I(Weight^10) , train)

m10

t10=sum(m10$residuals^2)
t10
pred = predict(m10, newdata=test)
e10=sum((pred-test$MPG)^2)
Rmse10=sqrt(mean(e10))
Rmse10

plot(train$Weight,train$MPG, pch=19, cex=0.5)
lines(sort(train$Weight), fitted(m10)[order(train$Weight)], col='brown', type='l',pch=20) 






trainerr=c(t1,t2,t7,t8,t9,t10)
RMSE=c(Rmse1,Rmse2,Rmse7,Rmse8,Rmse9,Rmse10)
trainerr


complexity= c(1,2,7,8,9,10)
for (i in RMSE){
plot(complexity,RMSE)
lines(complexity,RMSE,col="red",type="l")

}

complexity= c(1,2,7,8,9,10)
for (i in RMSE){
  plot(complexity,trainerr)
  
  #lines(testerr,col="red",type="l")
  lines(RMSE, complexity,col="blue",type="l")
  
}

for (i in complexity){
a=c(RMSE,trainerr)
plot(complexity,a, pch=19, cex=0.5)
lines(RMSE, col='red', type='l',pch=20) 
lines(testerr, col='blue', type='l',pch=20) 
}







