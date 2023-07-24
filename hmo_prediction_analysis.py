
''' DIVIDING DATASET INTO TRAINING AND TESTING SET
    We are dividing 70% of dataset into training data and 30% of dataset into testing data'''
    
#In this section, we divide our dataset into training and testing data for further analysis.

install.packages('caret')
library('caret')
trainlist <- createDataPartition(y=HMO_data_new$cost, p=0.70, list=FALSE)
trainSet <- HMO_data_new[trainlist,]
testSet <- HMO_data_new[-trainlist,]

#Variable analysis contained in the trainSet
str(trainSet)

#Variable analysis contained in the testSet 
str(testSet)


''' PREDICTION ANALYSIS OF "expensive" VARIABLE
    We will be using following model for the prediction:
    1. SVM
    2. K-SVM
    3. rpart '''
    

#First, we will use the SVM model where we apply the 'svmRadial' method. This is the most popular
#and most commonly used method as this is similar to a Gaussian distribution.
install.packages('kernlab')
library('caret')

#Training the SVM model on our train dataset.
hmo_svm <- train(expensive ~. , data=trainSet, method = "svmRadial", trControl = trainControl(method = "none"), preProcess = c("center", "scale"))
hmo_svm

#We will now test the model by predicting values in our test dataset.
pred_out <- predict(hmo_svm, newdata=testSet)
conf_matrix <- table(pred_out, testSet$expensive)

#Confusion matrix of the prediction.
conf_matrix

#As we see here, the error (1-accuracy) rate is ...

error <- (sum(conf_matrix) - sum(diag(conf_matrix)))/sum(conf_matrix)
accuracy <- 1- error
accuracy

#Here we use the confusionMatrix function from the caret package.
confusionMatrix(pred_out, testSet$expensive)

# KSVM Model
#Second, we will use the K-SVM model. This is a Kernel-SVM approach. The difference here as
#compared to the normal SVM approach is that we specify the number of Kernels (points) that
#we will use closest to the current point to determine if they are similar or not for classification.
#This method uses the efficiency of SVM along with the accuracy of KNN (nearest neighbor) method.

library(kernlab)

#Training the model on train dataset
ksvmHMO <- ksvm(expensive ~ ., data=trainSet, C=5, cross=3, prob.model=TRUE)
ksvmHMO

#Now we predict using the model on our test dataset.
ksvmPred <- predict(ksvmHMO, newdata=testSet)
ksvm_conf_matrix <- table(ksvmPred, testSet$expensive)
#Confusion Matrix
ksvm_conf_matrix

#As we see, the accuracy for this model increased as compared to SVM. It is around ............

ksvmError <- (sum(ksvm_conf_matrix) - sum(diag(ksvm_conf_matrix)))/sum(ksvm_conf_matrix)
ksvmAccuracy <- 1- ksvmError
ksvmAccuracy

#Using the confusionMatrix function to verify our result in the above block.
confusionMatrix(ksvmPred, testSet$expensive)

#rpart model 

install.packages("rpart")
install.packages("rpart.plot")

library(rpart)
library(rpart.plot)

HMOtreeModel <- train(expensive ~ ., data=trainSet,  method="rpart")

rpart.plot(HMOtreeModel$finalModel)

rpartHMOPred <- predict(HMOtreeModel, testSet)
confusionrPartHMO <- confusionMatrix(rpartHMOPred, testSet$expensive)
confusionrPartHMO