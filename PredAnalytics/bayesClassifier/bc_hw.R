library(e1071)
library(caret)

data <- read.csv("vote2.csv")

n <- dim(data)[1]

t1 <- sample(1:n, n*.8)
t2 <- setdiff(1:n, t1)

cl <- data[t2,]$party

train <- subset(data[t1,])
test <- subset(data[t2,], select =- party)

model <- naiveBayes(party ~., data=train)

pred <- predict(model, test)

confusionMatrix(pred, cl)
