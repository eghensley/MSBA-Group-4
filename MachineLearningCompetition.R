############################################
## Team 9 Labs - Bagging & Random Forests ##
############################################
rm(list=ls())

library (randomForest)

setwd("C:/Users/Eric/Desktop")
data <- read.csv("Lab3Data.csv")
data <- data[complete.cases(data), ]
data <- data[,-1]
attach(data)
data[,20] <- ifelse(Churn == "No", 0, ifelse(Churn == "Yes", 1, 2))
data[,1] <- ifelse(gender == "Male", 0, ifelse(gender == "Female", 1, 2))
data[,3] <- ifelse(Partner == "No", 0, ifelse(Partner == "Yes", 1, 2))
data[,4] <- ifelse(Dependents == "No", 0, ifelse(Dependents == "Yes", 1, 2))
data[,6] <- ifelse(PhoneService == "No", 0, ifelse(PhoneService == "Yes", 1, 2))
data[,7] <- ifelse(MultipleLines == "No", 1, ifelse(MultipleLines == "Yes", 2, 0))
data[,8] <- ifelse(InternetService == "Fiber optic", 1, ifelse(InternetService == "DSL", 2, 0))
data[,9] <- ifelse(OnlineSecurity == "No", 0, ifelse(OnlineSecurity == "Yes", 1, 2))
data[,10] <- ifelse(OnlineBackup == "No", 0, ifelse(OnlineBackup == "Yes", 1, 2))
data[,11] <- ifelse(DeviceProtection == "No", 0, ifelse(DeviceProtection == "Yes", 1, 2))
data[,12] <- ifelse(TechSupport == "No", 0, ifelse(TechSupport == "Yes", 1, 2))
data[,13] <- ifelse(StreamingTV == "No", 0, ifelse(StreamingTV == "Yes", 1, 2))
data[,14] <- ifelse(StreamingMovies == "No", 0, ifelse(StreamingMovies == "Yes", 1, 2))
data[,16] <- ifelse(PaperlessBilling == "No", 0, ifelse(PaperlessBilling == "Yes", 1, 2))
data[,15] <- ifelse(Contract == "One Year", 0, ifelse(Contract == "Two Year", 1, ifelse(Contract == "Month-to-month", 2, 3)))
data[,17] <- ifelse(PaymentMethod == "Credit card (automatic)", 0, ifelse(PaymentMethod == "Bank Transfer (automatic)", 1, ifelse(PaymentMethod == "Electronic check", 2, ifelse(PaymentMethod == "Mailed check", 3, 4))))


set.seed(1108)
train <- sample(1:nrow(data), (nrow(data)/5)*4)
test <- data[-train,"Churn"]

bag = randomForest(Churn~., data=data, subset=train, mtry=19, importance =TRUE)
yhat.bag = predict(bag, newdata=data[-train ,])
mse1 = mean((yhat.bag-test)^2)



bag.1 = randomForest(Churn~., data=data, subset=train, mtry=19, ntree=25)
yhat.bag.1 = predict(bag.1, newdata=data[-train ,])
mse2 = mean((yhat.bag.1-test)^2)
set.seed (1108)



rf.data = randomForest(Churn~., data=data, subset=train , mtry=12, importance=TRUE)
yhat.rf = predict(rf.data, newdata=data[-train ,])
mse3 = mean((yhat.rf-test)^2)
importance(rf.data)




rf.data.1 = randomForest(Churn~., data=data, subset=train , mtry=7, importance=TRUE)
yhat.rf.1 = predict(rf.data.1, newdata=data[-train ,])
mse4 = mean((yhat.rf.1-test)^2)




test_categorical = c()
length.test = length(test)
for (i in 1:length.test) {
  if (yhat.bag[i] >= .5) {
    test_categorical <- rbind(test_categorical, "Yes")
  }else {
    test_categorical <- rbind(test_categorical, "No")
  }
}


output <- matrix(nrow=0, ncol=3)
output <- rbind(output, c("model", "correct rate", "MSE"))

results1 = c()
for (i in 1:length.test) {
if (yhat.bag[i] >= .5) {
  results1 <- rbind(results1, "Yes")
}else {
  results1 <- rbind(results1, "No")
}
}
mytable1<-table(test_categorical,results1)
output <- rbind(output, c("model 1", (mytable1["Yes","Yes"]+mytable1["No","No"])/sum(mytable1), mse1))

results2 = c()
length.test = length(yhat.bag.1)
for (i in 1:length.test) {
  if (yhat.bag.1[i] >= .5) {
    results2 <- rbind(results2, "Yes")
  }else {
    results2 <- rbind(results2, "No")
  }
}
mytable2<-table(test_categorical,results2)
output <- rbind(output, c("model 2", (mytable2["Yes","Yes"]+mytable2["No","No"])/sum(mytable2), mse2))


results3 = c()
length.test = length(yhat.rf)
for (i in 1:length.test) {
  if (yhat.rf[i] >= .5) {
    results3 <- rbind(results3, "Yes")
  }else {
    results3 <- rbind(results3, "No")
  }
}
mytable3<-table(test_categorical,results3)
output <- rbind(output, c("model 3", (mytable3["Yes","Yes"]+mytable3["No","No"])/sum(mytable3), mse3))


results4 = c()
length.test = length(yhat.rf.1)
for (i in 1:length.test) {
  if (yhat.rf.1[i] >= .5) {
    results4 <- rbind(results4, "Yes")
  }else {
    results4 <- rbind(results4, "No")
  }
}
mytable4<-table(test_categorical,results4)
output <- rbind(output, c("model 4", (mytable4["Yes","Yes"]+mytable4["No","No"])/sum(mytable4), mse4))






training_split = sample(1:nrow(data), (nrow(data)/5)*4)
train = data[training_split, ]
test = data[-training_split, ]

test_cat = test
attach(test_cat)
test_cat[,20] <- ifelse(Churn == 0, "No", ifelse(Churn == 1, "Yes", "idk")) 


library(gbm)
set.seed(1108)
pows = seq(-10, -0.2, by = 0.1)
lambdas = 10^pows
length.lambdas = length(lambdas)
train.errors = rep(NA, length.lambdas)
test.errors = rep(NA, length.lambdas)
test.overall.error = rep(0, length.lambdas)
length.test = nrow(test)
for (i in (length.lambdas/2):length.lambdas) {
  boost = gbm(Churn ~ ., data = train, distribution = "gaussian", 
              n.trees = 1000, shrinkage = lambdas[i])
  train.pred = predict(boost, train, n.trees = 1000)
  test.pred = predict(boost, test, n.trees = 1000)
  train.errors[i] = mean((train$Churn - train.pred)^2)
  test.errors[i] = mean((test$Churn - test.pred)^2)
  results = c()
  for (j in 1:length.test) {
    if (test.pred[j] >= .5) {
      results <- rbind(results, "Yes")
    }else {
      results <- rbind(results, "No")
    }
  }
  mytable <-table(test_cat[,20],results)
  tryCatch({
    test.overall.error[i] = (mytable["Yes","Yes"]+mytable["No","No"])/sum(mytable)},
    error=function(e){})
}

max(test.overall.error)
best.lambda = lambdas[which.max(test.overall.error)]
test.overall.error[which.max(test.overall.error)]
test.errors[which.max(test.overall.error)]





best.train.errors = 0
best.test.errors = 0
best.test.overall.error = 0
boost.best = gbm(Churn ~ ., data = train, distribution = "gaussian", n.trees = 1000, shrinkage = best.lambda)
train.pred.boost = predict(boost.best, train, n.trees = 1000)
test.pred.boost = predict(boost.best, test, n.trees = 1000)
boost.train.errors = mean((train$Churn - train.pred.boost)^2)
boost.test.errors = mean((test$Churn - test.pred.boost)^2)
results.boost = c()
for (j in 1:length.test) {
  if (test.pred.boost[j] >= .5) {
    results.boost <- rbind(results.boost, "Yes")
  }else {
    results.boost <- rbind(results.boost, "No")
  }
}
mytable.boost <-table(test_cat[,20],results.boost)
boost.test.overall.error = (mytable.boost["Yes","Yes"]+mytable.boost["No","No"])/sum(mytable.boost)
mse5 = boost.test.errors

output <- rbind(output, c("model 5", boost.test.overall.error, mse5))


data_test <- read_csv("C:/Users/Eric/Desktop/ChurnDataTest.csv")

data_test <- data_test[complete.cases(data_test), ]
data_test <- data_test[,-1]
attach(data_test)
data_test[,1] <- ifelse(gender == "Male", 0, ifelse(gender == "Female", 1, 2))
data_test[,3] <- ifelse(Partner == "No", 0, ifelse(Partner == "Yes", 1, 2))
data_test[,4] <- ifelse(Dependents == "No", 0, ifelse(Dependents == "Yes", 1, 2))
data_test[,6] <- ifelse(PhoneService == "No", 0, ifelse(PhoneService == "Yes", 1, 2))
data_test[,7] <- ifelse(MultipleLines == "No", 1, ifelse(MultipleLines == "Yes", 2, 0))
data_test[,8] <- ifelse(InternetService == "Fiber optic", 1, ifelse(InternetService == "DSL", 2, 0))
data_test[,9] <- ifelse(OnlineSecurity == "No", 0, ifelse(OnlineSecurity == "Yes", 1, 2))
data_test[,10] <- ifelse(OnlineBackup == "No", 0, ifelse(OnlineBackup == "Yes", 1, 2))
data_test[,11] <- ifelse(DeviceProtection == "No", 0, ifelse(DeviceProtection == "Yes", 1, 2))
data_test[,12] <- ifelse(TechSupport == "No", 0, ifelse(TechSupport == "Yes", 1, 2))
data_test[,13] <- ifelse(StreamingTV == "No", 0, ifelse(StreamingTV == "Yes", 1, 2))
data_test[,14] <- ifelse(StreamingMovies == "No", 0, ifelse(StreamingMovies == "Yes", 1, 2))
data_test[,16] <- ifelse(PaperlessBilling == "No", 0, ifelse(PaperlessBilling == "Yes", 1, 2))
data_test[,15] <- ifelse(Contract == "One Year", 0, ifelse(Contract == "Two Year", 1, ifelse(Contract == "Month-to-month", 2, 3)))
data_test[,17] <- ifelse(PaymentMethod == "Credit card (automatic)", 0, ifelse(PaymentMethod == "Bank Transfer (automatic)", 1, ifelse(PaymentMethod == "Electronic check", 2, ifelse(PaymentMethod == "Mailed check", 3, 4))))


training_split = sample(1:nrow(data), (nrow(data)/1)*1)
train = data[training_split, ]

boost.best = gbm(Churn ~ ., data = train, distribution = "gaussian", 
            n.trees = 1000, shrinkage = best.lambda)
final.results <- predict(boost.best, newdata=data_test, n.trees = 1000)
final_results = c()
for (k in 1:length(final.results)) {
  if (final.results[k] >= .5) {
    final_results <- rbind(final_results, "Yes")
  }else {
    final_results <- rbind(final_results, "No")
  }
}

final_results
output