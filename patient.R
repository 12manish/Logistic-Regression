getwd()
setwd("E:\\Jigshaw lectures\\R STUDIO CLASS\\regression\\set2")
patient<-read.csv("Patient Data.csv")
View(patient)
#heart failure is already mentioned in 1 and 0 numeric form no need to change 

summary(patient)



1># What is the ratio of heart failure  vs. not heart failure?

# Frequency Distribution of the binary dependent variable 

9012+1788
9012/10800=0.83
table(patient$HEARTFAILURE)
table(patient$HEARTFAILURE)/nrow(patient)
  #0.16 people died by heart failure

#2>checking missing value
sum(is.na(patient))


#3> Partition the dataset into training and validation dataset  #
#MAX NO. OF ROWS THEN DIVIDE (TRAIN & TEST)

sampling<-sort(sample(nrow(patient),nrow(patient)*0.7))

dim(patient)   #10800 of 70% =1080*7= 7560
class(sampling)  #just like vector to find length use length function
length(sampling)
#Row subset and create training and validation samples using the index numbers
train<-patient[sampling,]
test<-patient[-sampling,]
nrow(train)
nrow(test)


#4>#Are any of the independent variables correlated?  #MULTICOLLINEARITY IF IT IS THERE DROP
library(corrplot)
#Finding correlation between numeric variables 

str(train)
colnames(train)

traincor<-cor(train[,c(1,2,3,4,6,10)])   # take continuos variable 
class(traincor)    #when we do coorelation data frame is converted into matrix
library(corrgram)    
?corrgram
cormat<-corrgram(traincor)  #graphical display of a corelation matrix
?corrplot
corplot_1<-corrplot(traincor)   #graphical display of a corelation matrix(confidence interval)



#Now glm (generalized linear model):used for logistic
?glm
colnames(train)

model<-glm(HEARTFAILURE ~.,family = "binomial",data = train)
summary(model)


#Gives best fitted model
#To choose a good model  (step function)

step(model,direction = "both")
model1<-glm(formula = HEARTFAILURE ~ AVGHEARTBEATSPERMIN + PALPITATIONSPERDAY + 
             CHOLESTEROL + BMI + FAMILYHISTORY + SMOKERLAST5YRS + EXERCISEMINPERWEEK, 
           family = "binomial", data = train)
summary(model1)
str(model1)
str(patient)

# Creating dummy variables   #we are taking 3 variable frequently reraley those factor variable
#train$FAMILYHISTORY_s<-ifelse(train$FAMILYHISTORY == "Y",1,0)
#train$SMOKERLAST5YRS_s<-ifelse(train$SMOKERLAST5YRS == "Y",1,0)
#summary(model1)
#library(dplyr)
#train%>%filter(SMOKERLAST5YRS_s== "1")%>%nrow
model2<-glm(formula = HEARTFAILURE ~ AVGHEARTBEATSPERMIN + PALPITATIONSPERDAY + 
               + BMI + FAMILYHISTORY + SMOKERLAST5YRS + EXERCISEMINPERWEEK, 
            family = "binomial", data = train)
summary(model2)

#levels(train$SMOKERLAST5YRS)


train$SMOKERLAST5YRS_S<-ifelse(train$SMOKERLAST5YRS == "Y",1,0)
train$FAMILYHISTORY_s<-ifelse(train$FAMILYHISTORY == "Y",1,0)



test$SMOKERLAST5YRS_S<-ifelse(test$SMOKERLAST5YRS == "Y",1,0)
test$FAMILYHISTORY_s<-ifelse(test$FAMILYHISTORY == "Y",1,0)

#to gauge the performnace [kappa and confusion matrix]

#test$pred<-predict(model2,type = "response", newdata = test)
#head(test$pred)

train$predicted <- model2$fitted.values   #probabilty of attrition of 1
train$predicted

# Compare with actual data

head(train$HEARTFAILURE)

head(train$predicted)


library(ROCR)


#predection and performance

pred<-prediction(train$predicted,train$HEARTFAILURE)
pred
class(pred)      #ROCR :- Receiver operating charcteristics curve

?performance

perf <- performance(pred,"acc")
class(perf)
perf

cutoffprob <- as.numeric(unlist(perf@x.values))

cutoffprob

class(perf@y.values)
accuracies <- as.numeric(unlist(perf@y.values))
accuracies



cutoffsvsacc <- data.frame(cutoffprob, accuracies )
cutoffsvsacc

# In the decreasing order of accuracy
cutoffsvsacc <- cutoffsvsacc[order(cutoffsvsacc$accuracies, decreasing=TRUE),]

# Pick cutoff for which Accuracy is highest 

train$predclass <- ifelse(train$predicted>0.5395044,1,0)



# Kappa values and Confusion Matrix from caret package
install.packages("caret")
library(caret)
install.packages("irr")
library(irr)


kappa2(data.frame(train$HEARTFAILURE,train$predclass))
confusionMatrix(as.factor(train$HEARTFAILURE),as.factor(train$predclass), positive = "1")

## computing a simple ROC curve (x-axis: fpr, y-axis: tpr)

perf<-performance(pred,"tpr","fpr") #tpr=TP/P fpr=FP/N
plot(perf,col="red")


abline(0,1, lty = 8, col = "blue")

auc<-performance(pred,"auc")
auc



#now i am  looking for test
#glm
mod<-glm(data=test,family = "binomial",HEARTFAILURE ~.)
summary(mod)
step(mod,direction = "both")
mod1<-glm(formula = HEARTFAILURE ~ AVGHEARTBEATSPERMIN + PALPITATIONSPERDAY + 
      FAMILYHISTORY + SMOKERLAST5YRS + EXERCISEMINPERWEEK
      , family = "binomial", data = test[,-c(11:14)])
summary(mod1)
nrow(test)
test<-test[,-c(11:14)]
test$SMOKERLAST5YRS_S1<-ifelse(test$SMOKERLAST5YRS == "Y",1,0)
test$FAMILYHISTORY_S2<-ifelse(test$FAMILYHISTORY == "Y",1,0)
step(mod,direction = "both")
mod2<-glm(formula = HEARTFAILURE ~ AVGHEARTBEATSPERMIN + PALPITATIONSPERDAY + 
            BMI + EXERCISEMINPERWEEK + SMOKERLAST5YRS_S1
          + FAMILYHISTORY_S2,family = "binomial", data = test)
summary(mod2)


#now looking for performance
test$predicted<-mod2$fitted.values
test$predicted


train$predicted <- model2$fitted.values   #probabilty of attrition of 1
train$predicted
?fitted.values
# Compare with actual data

head(test$HEARTFAILURE)

head(test$predicted)


library(ROCR)


#predection and performance

pred<-prediction(test$predicted,test$HEARTFAILURE)
pred    #rows of tp tn fp fn
class(pred)      #ROCR :- Receiver operating charcteristics curve

?performance

perf <- performance(pred,"acc")
class(perf)
perf   #An object of class "performance"
               Slot "x.name":
              [1] "Cutoff"

                Slot "y.name":
               [1] "Accuracy"

              Slot "alpha.name":
                [1] "none"

cutoffprob <- as.numeric(unlist(perf@x.values))

cutoffprob

class(perf@y.values)
accuracies <- as.numeric(unlist(perf@y.values))
accuracies



cutoffs <- data.frame(cutoffprob, accuracies )
cutoffs

# In the decreasing order of accuracy
cutoffs <- cutoffs[order(cutoffs$accuracies, decreasing=TRUE),]

# Pick cutoff for which Accuracy is highest 

test$predclass <- ifelse(test$predicted>0.454755,1,0)



# Kappa values and Confusion Matrix from caret package
install.packages("caret")
library(caret)
install.packages("irr")
library(irr)


kappa2(data.frame(test$HEARTFAILURE,test$predclass))
confusionMatrix(as.factor(test$HEARTFAILURE),as.factor(test$predclass))

## computing a simple ROC curve (x-axis: fpr, y-axis: tpr)

perf<-performance(pred,"tpr","fpr") #tpr=TP/P fpr=FP/N
plot(perf,col="red")


abline(0,1, lty = 8, col = "blue")

auc<-performance(pred,"auc")
auc
