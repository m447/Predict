library(caret)
fileUrl_train <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
download.file(fileUrl, destfile = "./pml-training.csv")
fileUrl_test <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
download.file(fileUrl, destfile = "./pml0testing.csv")
training <- read.csv("./pml-training.csv")
testing <- read.csv("./pml-testing.csv")

training <- training[ ,-c(1:7)]
testing <- testing[ ,-c(1:7)]


length(training[training=="#DIV/0!"])
nrow(training[complete.cases(training),])

training[training=="#DIV/0!"] <- 0
testing[testing=="#DIV/0!"] <- 0
training[is.na(training)] <- 0
testing[is.na(testing)] <- 0

nrow(training[complete.cases(training),])
nrow(testing[complete.cases(testing),])

training <- training[complete.cases(training),]
training <- training[,-nearZeroVar(training)]


inTrain <- createDataPartition(y=training$classe, p=0.7, list=FALSE)
train <- training[inTrain,]
test <- training[-inTrain,]

train$classe <- as.factor(train$classe)
test$classe <- as.factor(test$classe)


preP <- preProcess(train[,-length(train)], method=c("scale","center","pca"), thresh=0.9)

fit <- train(classe ~., data = predict(preP, train), method = "knn")
prediction <- predict(fit, predict(preP, test))
confusionMatrix(prediction, test$classe)$overall[1]

table(testing$classe)

testing$classe <- predict(fit, predict(preP, testing))

