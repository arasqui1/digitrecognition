#setwd("C:/Users/arasqui1/Downloads/R programs/digitrecognizer")

install.packages("data.table")
install.packages("caret")
install.packages("e1071")
install.packages("NeuralNetTools")

library(data.table)
library(caret)
library(e1071)
library(NeuralNetTools)

train.dt <- data.table(read.csv("train.csv"))
test.dt <- data.table(read.csv("test.csv"))

#split into training and validation
idx <- createDataPartition(train.dt$label, times = 1, p = 0.7, list = FALSE)

train <- train.dt[idx, ]  #training data
validation <- train.dt[-idx, ] #validation data

# nzv <- nearZeroVar(train) #remove columns with near zero variance

#train.dx <- train[, -nzv, with = FALSE]  #retain columns with variance

#pca <- preProcess(train.dx, method = "pca")

train$label <- as.factor(train$label)

#grid <- expand.grid(size = 5, decay = .1) #hyper-parameters

nnet.mod <- train(label ~ ., data = train, 
                  method = "nnet",
                  preProcess = c("nzv", "pca"))   #takes too long 

plot(nnet.mod)
plotnet(nnet.mod)

#test on the validation set
pred <- predict(nnet.mod, validation)

#confusion matrix
cf.matrix <- as.matrix(table(Predicted = pred, Actual = validation$label))  #confusion matrix

n <- sum(cf.matrix)
true.pos <- diag(cf.matrix)
rowsum <- apply(cf.matrix, 1, sum)
colsum <- apply(cf.matrix, 2, sum)

#Performance metrics

accuracy <- sum(true.pos) / n
precision <- true.pos / rowsum
recall <- true.pos / colsum
F1 <- 2 * ( precision * recall / (precision + recall) ) 

data.frame(label = 0:9, accuracy, precision, recall, F1)

#Predicting on the test set
test.label <- predict(nnet.mod, test.dt)

#tune hyper parameters for better performance


