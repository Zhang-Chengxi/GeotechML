library(e1071)
library(rpart)
data(Ozone,package = "mlbench")
index = 1:nrow(Ozone)

mse_svm = numeric(10)
mse_rpart = numeric(10)
for (i in 1:10)
{
testindex = sample(index, trunc(length(index)/3))
testset = na.omit(Ozone[testindex,-3])
trainset = na.omit(Ozone[-testindex,-3]) 

svm.model = svm(V4 ~ ., data = trainset, cost = 1000, gamma = 0.0001)
svm.pred = predict(svm.model, testset[,-3])
mse_svm[i]=crossprod(svm.pred - testset[,3]) / length(testindex)

rpart.model = rpart(V4 ~ ., data = trainset)
rpart.pred = predict(rpart.model, testset[,-3])
mse_rpart[i]= crossprod(rpart.pred - testset[,3]) / length(testindex)

}