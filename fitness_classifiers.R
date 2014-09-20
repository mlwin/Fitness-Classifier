
library(caret); 
library(randomForest); 

# load data files
if(!file.exists("trainData.csv")) 
{
  download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", 
                destfile= "./trainData.csv")
}

if(!file.exists("testData.csv")) 
{
  download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", 
                destfile= "./testData.csv")
}

trainData = read.csv("trainData.csv", na.strings = c('NA','#DIV/0!',''), stringsAsFactors=FALSE)
testData  = read.csv("testData.csv",  na.strings = c('NA','#DIV/0!',''), stringsAsFactors=FALSE )

## Feature selection and cross validation

# include only data with new_window == "no", which indicates that it is a sensor reading and not statistical derivations of the reading
trainData = trainData[trainData$new_window == "no", ]


# clean data by identifyng predictors with near zero variance in the training data set
nZeroVar      = nearZeroVar(trainData, saveMetrics=TRUE)
ignoreColumns = row.names(nZeroVar[nZeroVar$nzv==TRUE, ])


# ignore the following columns as they are not predictors for the manner in which they did the exercise
ignoreColumns = append(ignoreColumns, c("X", "user_name", "raw_timestamp_part_1", "raw_timestamp_part_2", "cvtd_timestamp", "num_window"))


trainData = trainData[, -which(names(trainData) %in% ignoreColumns)]
testData  = testData[, -which(names(testData) %in% ignoreColumns)]

trainData$classe <-as.factor(trainData$classe)

dim(trainData)

# Slice the provided training data into training subset and test subset.  
# This is done to get an idea of what the out of sample error in model selection before the validation with final test data set.
# Since traing data set is medium size, using the rules of thumb for prediction study design as describe in the class lecture, 60% is partition for training and 40% for testing.
set.seed(643216)
inTrain <- createDataPartition (y= trainData$classe, p=0.6, list = FALSE)
trainData.training = trainData[inTrain,]
trainData.testing  = trainData[-inTrain,]

# To limit the computation time, trainControl() is used to set the number of cross-validations.

# the number of cross-validations to 5 
trainCtrl = trainControl(method = "cv", number = 3)

if (file.exists("rf.model")) {
  load( "rf.model" )
} else {
  modelRf <- train(classe~., method="rf", trControl=trainCtrl, data=trainData.training, prox=TRUE) 
  save(file="rf.model", "modelRf")
}

p = predict(modelRf, trainData.testing)
cm = confusionMatrix(p, trainData.testing$classe)
cm$overall[1]

## Test Data Validation
test.predict = predict(modelRf, testData)
test.predict

