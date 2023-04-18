#customer churn analysis
#Stephanie Vu

library(plyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(MASS)
library(randomForest)
library(party)
library(caret)

purchase <- read.csv("C:\\STEPHY\\R\\somefakepath\\ML Milktea Final.csv")
str(purchase)


#change character features to factors
purchase$Likelytobuy <- as.factor(purchase$Likelytobuy)
purchase$Age <- as.factor(purchase$Age)
purchase$Ethnicity <- as.factor(purchase$Ethnicity)
purchase$Gender <- as.factor(purchase$Gender)
purchase$Annualhouseholdincome <- as.factor(purchase$Annualhouseholdincome)


### split train test set 70% train 30% test
set.seed(2017)
intrain<- createDataPartition(purchase$Likelytobuy,p=0.7,list=FALSE)
training<- purchase[intrain,]
testing<- purchase[-intrain,]
str(training)

#build the decision tree
tree <- ctree(Likelytobuy~Age+Ethnicity+Gender+Annualhouseholdincome, training, controls = ctree_control(mincriterion = 0.001, minsplit = 1, minbucket = 1))
plot(tree)
