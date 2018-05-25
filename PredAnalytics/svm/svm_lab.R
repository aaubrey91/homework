library(e1071)
library(caret)

data <- read.csv(file='/Users/alexaubrey/Documents/School/MSDS/PredAnalytics/inclass_assignments/svm/vote2.csv',
                 head=TRUE, sep=",")

t1 = sample(1:435, 348)
t2 = setdiff(1:435, t1)
train = subset(data[t1,])
test = subset(data[t2,], select =-party)

mod <- svm(party ~ ., data=train)

cl <- data[t2,]$party

pred <- predict(mod, test)
pred

confusionMatrix(pred, cl)

summary(mod)

library(caret)

y = train$party
x = subset(train, select=-party)

P_model <- train(x,y, method="svmPoly", tuneLength=5,
                 trControl=trainControl(method='repeatedcv', number=10, repeats=10))

L_model <- train(x,y, method="svmLinear", tuneLength=5,
                 trControl=trainControl(method='repeatedcv', number=10, repeats=10))

R_model <- train(x,y, method="svmRadial", tuneLength=5,
                 trControl=trainControl(method='repeatedcv', number=10, repeats=10))

P_model
L_model
R_model

pred <- predict(R_model, test)
confusionMatrix(pred, cl)
