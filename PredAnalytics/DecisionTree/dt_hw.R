library(rpart)
library(caret)

data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/glass/glass.data",
                 col.names=c("ID", "RI", "Na", "Mg", "Al","Si","K",
                             "Ca","Ba","Fe", "glass"))

#Convert class num to label
data$glass_name <- "NULL"
data[data$glass == 1,]$glass_name <- "building_windows_float_processed"
data[data$glass == 2,]$glass_name <- "building_windows_non_float_processed"
data[data$glass == 3,]$glass_name <- "vehicle_windows_float_processed"
data[data$glass == 4,]$glass_name <- "vehicle_windows_non_float_processed"
data[data$glass == 5,]$glass_name <- "containers"
data[data$glass == 6,]$glass_name <- "tableware"
data[data$glass == 7,]$glass_name <- "headlamps"

#Drop ID and the class number before we changed it
data$ID <- NULL
data$glass <- NULL

#Train/Test Split
n <- dim(data)[1]

t1 <- sample(1:n, n*.8)
t2 <- setdiff(1:n, t1)

train <- subset(data[t1,])
test <- subset(data[t2,], select =- glass_name)

### 1. BASIC ###
fitControl <- trainControl(method='cv', number=10)

Grid <- expand.grid(cp=seq(0,0.1, 0.005))
#Run the training, needed to remove NAs
trained_tree <- train(glass_name ~ ., data=train, method='rpart', 
                      trControl=fitControl, metric='Accuracy', maximize=TRUE, tuneGrid=Grid)
trained_tree

final_tree <- predict(trained_tree, test, type="raw")
confusionMatrix(final_tree, data[test,]$glass_name)



