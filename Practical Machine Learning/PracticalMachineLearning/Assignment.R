################################################################################

## Coursera Practical Machine Learning Course Project
## Ronan Mulligan
## 26 February 2017

################################################################################
# Load the required libraries
wants <- c("knitr", "caret", "rpart", "randomForest", "rpart.plot")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
lapply(wants, library, character.only=T)

# Set the working directory to where the data is stored
work_dir <- "C:/Users/User/DataScience/Practical Machine Learning/Assignment"
setwd(work_dir)

# load csv files
fileurl1 <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
download.file(fileurl1, destfile = "trainingData.csv")

fileurl2 <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(fileurl2, destfile = "testingData.csv")


trainingData <- read.csv("trainingData.csv", header = TRUE, 
                         na.strings = c("NA", "#DIV/0!", ""), strip.white=T)
testingData <- read.csv("testingData.csv", header = TRUE, 
                        na.strings = c("NA", "#DIV/0!", ""), strip.white=T)
# set seed
set.seed(11234)

# explore data structure
str(trainingData)

# create training and validation partition in the "training" dataset
inTrain  <- createDataPartition(trainingData$classe, p=0.7, list=FALSE)
trainSet <- trainingData[inTrain, ]
testSet <- trainingData[-inTrain, ]

# remove variables with Nearly Zero Variance
NZV <- nearZeroVar(trainSet)
trainSet <- trainSet[ , -NZV]
testSet <- testSet[ , -NZV]

# remove variables that are mostly NA
MostlyNA    <- sapply(trainSet, function(x) mean(is.na(x))) > 0.95
trainSet <- trainSet[, MostlyNA==FALSE]
testSet <- testSet[, MostlyNA==FALSE]

# remove identification only variables (columns 1 to 5)
trainSet <- trainSet[, -(1:5)]
testSet <- testSet[, -(1:5)]

# A randomForest model analysis, takes too long to run
controlRF <- trainControl(method="cv", number=4, verboseIter=FALSE)
modelRandForest <- train(classe ~ ., data=trainSet, model="rf", trControl=controlRF)
modelRandForest$finalModel

predictRandForest <- predict(modelRandForest, newdata=testSet)
confMatRandForest <- confusionMatrix(predictRandForest, testSet$classe)
confMatRandForest

varImp(modelRandForest)

# A randomForest model analysis with fewer variables (most important 10)
trainSetSmall <- trainSet[, c("classe", "num_window", "roll_belt", "pitch_forearm", 
                                      "magnet_dumbbell_y", "magnet_dumbbell_z",
                                      "pitch_belt", "yaw_belt", "roll_forearm", 
                                      "roll_dumbbell", "magnet_dumbbell_x")]

controlRFs <- trainControl(method="cv", number=4, verboseIter=FALSE)
modelRandForestsmall <- train(classe ~ . , data=trainSetSmall, model="rf", 
                              trControl=controlRF)
modelRandForestsmall$finalModel

predictRandForestsmall <- predict(modelRandForestsmall, newdata=testSet)
confMatRandForestsmall <- confusionMatrix(predictRandForestsmall, testSet$classe)
confMatRandForestsmall

