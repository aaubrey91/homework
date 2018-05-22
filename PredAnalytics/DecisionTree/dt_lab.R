library(rpart)
library(rattle)
library(caret)

data <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data",
                 col.names = c("ID", "Diagnosis","radius_m","texture_m","perimeter_m","area_m",
                               "smoothness_m","compactness_m", "concavity_m",
                               "concavePoints_m","symmetry_m","fractalDimension_m",
                               "radius_ste","texture_ste","perimeter_ste","area_ste",
                               "smoothness_ste","compactness_ste", "concavity_ste",
                               "concavePoints_ste","symmetry_ste","fractalDimension_ste",
                               "radius_w","texture_w","perimeter_w","area_w",
                               "smoothness_w","compactness_w", "concavity_w",
                               "concavePoints_w","symmetry_w","fractalDimension_w"),
                 header=FALSE)


train <- sample(1:569, 455)
test <- setdiff(1:569, train)

data_train = data[train,]
data_test = subset(data[test,], select =-Diagnosis)

rpartTree <- rpart(Diagnosis ~ ., data=data_train)

out = predict(rpartTree, data_test, type="class")

confusionMatrix(out, data[test,]$Diagnosis)

## Could not get Rattle Installed
#fancyRpartPlot(rpartTree)
rpart.plot::rpart.plot(rpartTree)

temp <- rpart.control(xval=10, minbucket = 2, minsplit = 4, cp = 0)
dfit <- rpart(Diagnosis ~ ., data=data_train, control=temp)

## Could not get Rattle Installed
#fancyRpartPlot(dfit)
rpart.plot::rpart.plot(dfit)

library(caret)
#10-folds cross validation
fitControl <- trainControl(method='cv', number=10)

Grid <- expand.grid(cp=seq(0,0.05, 0.005))
#Run the training, needed to remove NAs
trained_tree <- train(Diagnosis ~ ., data=data_train, method='rpart', 
                      trControl=fitControl, metric='Accuracy', maximize=TRUE, tuneGrid=Grid)
trained_tree

out2 = predict(trained_tree, data_test, type="raw")
confusionMatrix(out2, data[test, ]$Diagnosis)

### BAGGING ###
library(ipred)
baggedTree <- bagging(Diagnosis ~ ., data=data_train)
out3 = predict(baggedTree,data_test)
confusionMatrix(out3, data[test,]$Diagnosis)

baggedTree <- bagging(Diagnosis ~ ., data=data_train, nbagg = 4)

#needed to remove NAs in training set
mod <- train(Diagnosis ~ ., data=data_train, 
             method="treebag", 
             trControl=fitControl, 
             metric='Accuracy', 
             maximize=TRUE)

print(mod)


## Random Forest ##
library(randomForest)
rfModel <- randomForest(Diagnosis ~ ., data = data_train)
rfModel

out5 = predict(rfModel, data_test)
confusionMatrix(out5, data[test,]$Diagnosis)

rfModel <- randomForest(Diagnosis ~ ., data = data_train, ntree=10, mtry=4)
rfModel

control <- trainControl(method="repeatedcv", number = 10, repeats=3)
metric <- "Accuracy"
n <- round(sqrt(ncol(data_train)))
tunegrid <- expand.grid(.mtry=seq(4,n,1))
rf_default <- train(Diagnosis ~ ., data=data_train, method="rf", metric=metric, tuneGrid=tunegrid,
                    trainControl = control)
print(rf_default)


#variable importance
Imp <- varImp(rpartTree)
Imp

Imp <- varImp(mod)
Imp
