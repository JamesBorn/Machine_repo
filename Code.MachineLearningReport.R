
set.seed(12345)
library(hexbin)
library(randomForest)
library(caret)
library(ggplot2)
library(corrplot)
library(devtools)

#check if a data folder exists; if not then create one
if (!file.exists("data")) {dir.create("data")}


#file URL and destination file

fileUrl1 <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
destfile1 <- "./data/pml-training.csv"
fileUrl2 <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
destfile2 <- "./data/pml-testing.csv"

#download the file and note the time
download.file(fileUrl1, destfile = destfile1)
download.file(fileUrl2, destfile = destfile2)
dateDownloaded <- date()

#read the original training and testing data set

 

Original_training<- read.csv("./data/pml-training.csv", header = TRUE)
Original_test <- read.csv("./data/pml-testing.csv", header = TRUE)

#Cleaning data
#Firstly we have to replace all off these with and NA value with the R command line 
 
setwd("C:/Users/ryUser/Practical_Machine")
Original_training<- read.csv("pml-training.csv", header = TRUE,na.strings = c("NA", "","#DIV/0!"))
Original_test <- read.csv("pml-testing.csv",header = TRUE,na.strings = c("NA","","#DIV/0!"))
#Searching for missing values columns and Treating missing values
 

Original_training.NAS <- apply(Original_training, 2, function(x) {sum(is.na(x))})
Original_training.clean <- Original_training[,which(Original_training.NAS== 0)]
Original_test.NAS <- apply(Original_test, 2, function(x) {sum(is.na(x))})
Original_test.clean <- Original_test[,which(Original_test.NAS == 0)]

 

#Remove variable whicth are not predict movement like X (= row number) and 
#user_name (= the name of the subject) 

final_training_clean <- Original_training.clean[8:length(Original_training.clean)]
final_test_clean <- Original_test.clean[8:length(Original_test.clean)]

 #Build a Model
inTrain = createDataPartition(y = final_training_clean$classe, p = 0.6, list = FALSE)
Index_train = final_training_clean[inTrain, ]
validation = final_training_clean[-inTrain, ]
#plot a Bar charts show the distribution of categorical variable classe 
 
ggplot(Index_train) +geom_histogram(aes(x=classe),binwidth=5, fill="Blue")
 
#plot a correlation matrix
#The correlation plot lay in evidensce a significant correlation between the variables.
 
correlMatrix <- cor(Index_train[, -length(Index_train)])
corrplot(correlMatrix, order = "FPC", method = "circle", type = "lower", tl.cex = 0.8, tl.col = rgb(0, 0, 0))
#fit a model to predict the classe using everything else as a predictor
 
model <- randomForest(classe ~ ., data = Index_train, importance=TRUE)
#Examining variable importance
varImp <- importance(model)
#Plot the variable importance as measured by accuracy change
 
varImpPlot(model, type=1)
 # prediction
predict_CrossVal <- predict(model, validation)
confusionMatrix(validation$classe, predict_CrossVal)
#predict the classes of the test set
predictTest <- predict(model, final_test_clean )
predictTest


