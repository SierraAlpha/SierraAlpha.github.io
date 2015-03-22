setwd('/Users/marqueze/machinelearning')
library(caret)
lift<-read.csv('pml-training.csv', na.strings=c('NA', '#DIV/0!'))
naval<-which(as.numeric(colSums(is.na(lift)))>19200) #100 columns
newlift<- subset(lift, select = -(naval))
nzv<-nearZeroVar(newlift, saveMetrics=TRUE)
newlift<- subset(newlift, select = -(c(new_window, raw_timestamp_part_1, raw_timestamp_part_2,
                                       cvtd_timestamp, X, user_name)))
inTrain <- createDataPartition(newlift$classe, p = .75, list=FALSE)
training<-newlift[inTrain,]
test<-newlift[-inTrain,]

set.seed(1555)
crtl<-trainControl(method="cv", number=2)
modFit <- train(classe~ .,data=training, method="rf", prox=TRUE, trControl=crtl)
varImp(modFit)
crtl<-trainControl(method="cv", number=10)
modFit2<-train(classe ~ num_window + roll_belt + pitch_forearm + yaw_belt + 
                 magnet_dumbbell_z + magnet_dumbbell_y + pitch_belt + roll_forearm+
                 accel_dumbbell_y, data=training, method="rf", prox=TRUE,
               trControl=crtl)
confusionMatrix(test$classe, predict(modFit2, test))


testing<-read.csv('pml-testing.csv', na.strings=c('NA', '#DIV/0!'))
answers<-predict(modFit2, testing)
