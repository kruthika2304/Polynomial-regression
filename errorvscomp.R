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





trainerr=c(t1,t2)
testerr=c(Rmse1,Rmse2)

g = c(1, 3)
for (i in trainerr){
plot(g,testerr)
lines(g,testerr,col="red",type="l")

}

