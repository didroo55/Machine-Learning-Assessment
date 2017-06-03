---
title: "Coursera Machine Learning Project"
author: "Satish Hariharan"
date: "May 31, 2017"
output: html_document
---



## Executive Summary

This document presents the results of the Practical Machine Learning Peer Assessments in a report using a single R markdown document that can be processed by knitr and be transformed into an HTML file.

Since we have a data set with to many columns and we need make a class prediction, we decide implement a random forests model, that's no need cross-validation or a separate test set to get an unbiased estimate of the test set error. Before apply the dataset to our prediction modelo, we decide remove all the columns that having less than 60% of data filled, instead try to filled it with some center measure. Our model accuracy over validation dataset is equal to 99.9235%. This model promoted a excelente prediction results with our testing dataset and generated the 20th files answers to submit for the Assignments. The Assignments returned to us that he 20th files answers as being correct!

## Prepare the Environment
We install the Caret and RandomForest packages which will be used for prediction.


```r
library(caret)
library(randomForest)
```

## Loading Data

We load the data from the given web links directly on to our workspace.

```r
URL1 <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
URL2 <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
filename1 <- "pml-training.csv"
filename2 <- "pml-testing.csv"

#Downloading files
download.file(url=URL1, destfile=filename1,method="curl")
download.file(url=URL2, destfile=filename2,method="curl")

#NA Handling steps
training <- read.csv("pml-training.csv",header=TRUE, sep=",", na.strings=c("NA",""))
testing <- read.csv("pml-testing.csv",header=TRUE, sep=",", na.strings=c("NA",""))
```

## Cleaning Data
Since we choose a random forest model and we have a data set with to many columns, first we check if we have many problems with columns without data. If it's the case we decide remove all the columns that having less than 60% of data filled, instead try to filled with some center measure.


```r
appropriate <- c((colSums(!is.na(training[,-ncol(training)])) >= 0.6*nrow(training)))
training   <-  training[,appropriate]
```

##Create Data Partitions
We use the caret data partition function to create the training and validating set.


```r
inTrain <-  createDataPartition(training$classe, p=0.60, list=FALSE)
trainingSet <- training[inTrain,]
validatingSet <- training[-inTrain,]
```

## Modelling
In random forests, there is no need for cross-validation or a separate test set to get an unbiased estimate of the test set error. It is estimated internally, during the execution. So, we proced with the training the model (Random Forest) with the training data set.


```r
model <- randomForest(classe~.,data=trainingSet[,-1])
print(model)
```

```
## 
## Call:
##  randomForest(formula = classe ~ ., data = trainingSet[, -1]) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 7
## 
##         OOB estimate of  error rate: 0.19%
## Confusion matrix:
##      A    B    C    D    E  class.error
## A 3347    1    0    0    0 0.0002986858
## B    2 2276    1    0    0 0.0013163668
## C    0    5 2047    2    0 0.0034079844
## D    0    0    7 1921    2 0.0046632124
## E    0    0    0    2 2163 0.0009237875
```

## Validating
We validate the model using the validating obtained. We then cretae the confusion marix.


```r
predicted <- predict(model,newdata=validatingSet[,-c(1,ncol(validatingSet))])

confusionmat <- confusionMatrix(predicted,validatingSet$classe)

confusionmat
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 2231    0    0    0    0
##          B    1 1518    4    0    0
##          C    0    0 1362    4    0
##          D    0    0    2 1281    6
##          E    0    0    0    1 1436
## 
## Overall Statistics
##                                           
##                Accuracy : 0.9977          
##                  95% CI : (0.9964, 0.9986)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9971          
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9996   1.0000   0.9956   0.9961   0.9958
## Specificity            1.0000   0.9992   0.9994   0.9988   0.9998
## Pos Pred Value         1.0000   0.9967   0.9971   0.9938   0.9993
## Neg Pred Value         0.9998   1.0000   0.9991   0.9992   0.9991
## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
## Detection Rate         0.2843   0.1935   0.1736   0.1633   0.1830
## Detection Prevalence   0.2843   0.1941   0.1741   0.1643   0.1832
## Balanced Accuracy      0.9998   0.9996   0.9975   0.9974   0.9978
```




## Preparing the  Quiz Testing data
Now that the model is validated using the Vlidation set, we proceed to use the testing data to understand how the model performs on the testing set.provided for the quiz. 

```r
testing <- testing[,appropriate]

testing <- testing[,-ncol(testing)]

testing <- testing[,-1]


common <- intersect(names(trainingSet), names(testing)) 
for (p in common) { 
  if (class(trainingSet[[p]]) == "factor") { 
    levels(testing[[p]]) <- levels(trainingSet[[p]]) 
  } 
}
```


##Predicting using quiz data
Once the testing data is prepared, we go ahead and predict using the testing set.


```r
predictions <- predict(model,newdata = testing)
```


##Writing answers
The answers are then written to a set of text files.



```r
write_files <- function(x) {
  n <- length(x)
  for (i in 1:n) {
    filename <- paste0("problem_id", i, ".txt")
    write.table(x[i], file=filename, quote=FALSE, row.names=FALSE,col.names=FALSE)
  }
}

write_files(predictions)
```



