# one sample test
# Z test can be used for n>30

mu = 0
xbar = mean(mtcars$mpg)
n <- length(mtcars$mpg)
sigma  = sd(mtcars$mpg)
z <- sqrt(n)* (xbar-mu)/sigma
p_value  = round(2*pnorm(-abs(z)),4)
p_value
z
sigma

# t- test can be used for all 
# p> 0.05 we fail to reject null hypothesis

# t = (xbar - mu)/ (s/sqrt(n)
t.test(mtcars$mpg,mu=20)

# two sample test
# assumption:
# It is used to compare means of a dependent variable by grouped independent variable with two categories
# For example, we can compare exam score by sex of the students!
# Assumptions:
#     Dependent variable must follow the normal distribution for each category (Tests of normality)

#     Variance across independent Variable categories are not homogenous i.e. not equal (Test of equal variance)


with(mtcars,shapiro.test(mpg[am==0])) # assumption test 1, if p_value > 0.05, normal distribution
var.test(mpg ~ am, data = mtcars) # assumption 2: if p_value > 0.05 equal variance assumption holds true

# one way anova h)  = u1  = u2= u3

# assumption -> 
# 1. same as two-sample test,
      # i.e. normal distribution
          # homogenerous variance -> var.test fails so we use 
          # leveneTest(y~x, data) -> x must be categorical, pvalue > 0.05
# 2. Dependendent variable must be normally distributed
# 3. Variance accross categories must be same

# anova formula
summary(aov(mpg~gear,data=mtcars))

cov(mtcars$wt,mtcars$mpg)

# linear modeling
library(caret)
library(dplyr)
lm1 <- lm(mtcars$mpg~mtcars$wt)
lm1
summary(lm1)

mse <- mean(lm1$residuals^2)
mse

#evaluation matrix
#R-square – Explained variance (higher is better!)
# RMSE – Root of MSE (lower is better)
# MAE – Mean Absolute Error (lower is better)
# MAPE – Mean Absolute Percentage Error (lower is better)

#Define the mtcars data as “data”:
data <- mtcars
#Use random seed to replicate the result
set.seed(1234)
#Do random sampling to divide the cases into two independent samples
ind <- sample(2, nrow(mtcars), replace = T, prob = c(0.7, 0.3))
#Data partition
train.data <- data[ind==1,]
test.data <- data[ind==2,]

# linear model fit, prediction and cross-validation (MSE)
lm4 <- lm(mpg~wt, data = train.data)
library(dplyr)
library(caret)
predictions <- lm4 %>%
  predict(test.data)
data.frame(R2 = R2(predictions,
                   test.data$mpg),
           RMSE = RMSE(predictions,
                       test.data$mpg),
           MAE = MAE(predictions,
                     test.data$mpg))
summary(lm4)

# linear model fit, prediction and cross-validation loocv(leave one out cross validation) approach

train.control <- trainControl(method = "LOOCV")
# Train the model
model1 <- train(mpg ~wt, data = mtcars, method = "lm",
                trControl = train.control)
# Summarize the results
print(model1)

#Prediction with LOOCV:
predictions1 <- model1 %>%
  predict(test.data)
data.frame(R2 = R2(predictions1,
                   test.data$mpg),
           RMSE = RMSE(predictions1,
                       test.data$mpg),
           MAE = MAE(predictions1,
                     test.data$mpg))



# linear model fit, prediction and cross-validation K-fold approach
set.seed(123)
train.control <- trainControl(method = "cv", number= 10)
train.control <- trainControl(method = "cv", number= 10, repeats = 3 ) # if repeated k-folds cross validation
# Train the model
model2 <- train(mpg ~ wt, data = mtcars, method = "lm",
                trControl = train.control)
# Summarize the results
print(model2)

# prediction with k-folds CV

predictions2 <- model2 %>%
  predict(test.data)
data.frame(R2 = R2(predictions2,
                   test.data$mpg),
           RMSE = RMSE(predictions2,
                       test.data$mpg),
           MAE = MAE(predictions2,
                     test.data$mpg))


#=============================================
#multiple regression

mlr <- lm(mpg ~., data = mtcars)
summary(mlr)
library(car)
vif(mlr)

# • We need to drop the independent variable with highest VIF and run the model again until all the VIF <10!

#Removing “cyl” variable:
mlr2 <- lm(mpg ~
             hp+drat+wt+qsec+vs+am+gear+carb,
           data = mtcars)
summary(mlr1)
vif(mlr1)
# • We need to drop the independent variable with highest VIF and run themodel again until VIF <10!
#   • If all the VIF < 10 then we can interpret the model and od the predections


# polynomial regression

library(readxl)
covid_tbl_final <- read_excel("covid_tbl_final.xlsx")

plot(covid_tbl_final$Date,
     covid_tbl_final$Deaths_daily,
     main = "Daily Deaths: 23 Jan 2020 31 May 2021",
     xlab = "Date",
     ylab = "Daily Deaths")


#The problem is associated with the
# three outliers (all the missed deaths
#                 a priori added to the data on those
#                 3 days!)

#Cumulative deaths upto 398 cases i.e. 23 Feb 2021
plot.data <-
  covid_tbl_final[covid_tbl_final$SN <=398,]
#Plot with filtered data
plot(plot.data$Date,
     plot.data$Deaths_total,
     main = "Daily Covid Deaths,
Nepal: 23 Jan - 23 Feb 2021",
     xlab = "Date",
     ylab = "Daily Deaths")

# Let us fit a linear model in the filtered data
# (plot.data) using SN as time variable:

#Linear model:
lm <- lm(Deaths_total ~ SN, data =
           plot.data)
summary(lm)


#Plot with linear model
plot(SN, Deaths_total, data = plot.data,
     main = "Daily Covid Deaths, Nepal: 23
Jan - 23 Feb 2021",
     xlab = "Date",
     ylab = "Daily Deaths")
abline(lm(Deaths_total ~ SN, data =
            plot.data), col = "red", lwd=2)


# quadratic model
qlm <- lm(Deaths_total ~ poly(SN, 3,
                              raw=T), data = plot.data)
summary(qlm)

#Double quadratic linear model
dqlm <- lm(Deaths_total ~
             poly(SN, 4, raw=T), data =
             plot.data)
summary(dqlm)

#Fifth order polynomial fit
folm <- lm(Deaths_total ~ poly(SN,
                               5, raw=T), data = plot.data)
summary(folm)
# Higher polynomial fit will give
#Fifth order polynomial fit
folm <- lm(Deaths_total ~ poly(SN,
                               5, raw=T), data = plot.data)
summary(folm)
# Higher polynomial fit will give higher R-squared and lower residual standard error for decreasing cumulative deaths!


####=====================================KNN====================
library(e1071)
#Define the data
boston = MASS::Boston
#Check the structure
str(boston)
#Data partition
set.seed(123)
ind <- sample(2, nrow(boston),
              replace = T, prob = c(0.8, 0.2))
train.data <- boston[ind==1,]
test.data <- boston[ind==2,]

#Training data scaling
train_x = train.data[, -14]
train_x = scale(train_x)[,]
train_y = train.data[,14]


#Test (validation) data scaling
test_x = test.data[, -14]
test_x = scale(test.data[,-14])[,]
test_y = test.data[,14]

library( class)
#KNN regression, structure and prdiction
knnmodel = knnreg(train_x, train_y)
str(knnmodel)

# If we do it manually then we can start with the K that is close to number of features (variables)/3 i.e. 15/3 =
#   5 for regression based supervised learning and square root of number of variables for classification based supervised learning!

pred_y = predict(knnmodel, data.frame(test_x))

print(data.frame(test_y, pred_y))

mse = mean((test_y - pred_y)^2)
mae = caret::MAE(test_y, pred_y)
rmse = caret::RMSE(test_y, pred_y)


#KNN regression model validation plot:
  #Plot
  x = 1:length(test_y)
plot(x, test_y, col = "red", type = "l",
     lwd=2,
     main = "Boston housing test data
prediction")
lines(x, pred_y, col = "blue", lwd=2)
legend("topright", legend = c("original-
medv", "predicted-medv"),
       fill = c("red", "blue"), col = 2:3, adj =
         c(0, 0.6))
grid()



#===============Neural NETROWK=================

#Library neuralnet
library(neuralnet)
#NN Model
n <- neuralnet(Deaths_total ~ SN,
               data=plot.data,
               hidden = 1,
               linear.output=F)
 plot(n)
#Linear.output = F means the relationship between Y and X is non-   linear!

 
 # Single “hidden” layer for Y ~ X (2 hidden layers with 3 and 2 neurons)
 
 #Library neuralnet
 #NN model
 n <- neuralnet(Deaths_total ~ SN,
                data=plot.data,
                hidden = c(3,2),
                linear.output=F)
 ##NN Plot
plot(n)


#Data Partition
ind <- sample(2, nrow(plot.data),
              replace = T, prob = c(0.7,0.3))
trainset <- plot.data[ind==1,]
testset <- plot.data[ind==2,]

# MODELING

#NN model
nn <- neuralnet(Deaths_total ~
                  SN, data=trainset, hidden=c(3,1),
                linear.output=FALSE,
                threshold=0.01)
#Plot the NN model
plot(nn)

# Multilayer perceptron: Y ~ X (2 hidden layers with 3 and 1 neurons i.e. c(3,1))

#Test the resulting output
temp_test <- subset(testset, select = c('SN'))
head(temp_test)
#Prediction using compute for NN model with neuralnet package!
  nn.results <- compute(nn, temp_test)
  
  #Model validation
  results <- data.frame(actual =
                          testset$SN, prediction =
                          nn.results$net.result)
  results
  #Model Accuracy
  deviation=((results$actual-
                results$prediction)/results$actual)
  (accuracy=abs(mean(deviation)))
  (error=1-accuracy)  
  
  
#=====================logistic regression ======================
  
library(readr)

titanic <- read.csv('titanic.csv')
#Change the name of the data
# and remove the 3rd column
# containing names of the
# passengers
data <- titanic[,-3]

#Check the structure of the data
str(data)

#Pclass variable is imported as
# numeric. Pclass variable
# contained data of passengers in
# 1 st , 2 nd and 3 rd class
table(data$Pclass)
#Let us change it as factor variable
data$Pclass <-
  as.factor(data$Pclass)
#Check this variable
str(data$Pclass)


#Let us do the same for the sex variable as well
data$Sex <- as.factor(data$Sex)
str(data$Sex)
#Let us retain age as it is, we could scale it and


#Logistic regression with generalized linear model function
#family = binomial means dependent variable is binary: 0 and 1 coded
model.full <- glm(Survived ~.,
                    data=data, family = binomial)
summary(model.full)
# McFadden’s pseudo R-square
(mfpr2 <- 1 -
    (model.full$deviance/model.full$null.deviance))

#plot glm with age as independent variable
library(ggplot2)
ggplot(data, aes(x=Age,
                 y=Survived)) + geom_point() +
  stat_smooth(method="glm",
              family="binomial", se=FALSE)

# Confusion matrix of the logistic regression model of full dataset (statistical approach):
  #Prediction
predict <- predict(model.full, type="response")
#Prediction to binary variable
predcted.fm <- as.numeric(ifelse(predict>0.5,1,0))
#Confusion matrix
(cm <- table(predcted.fm,data$Survived))

# accuracy, error, sensitivity,

#Sensitivity, Specificity, Accuracy
(accuracy <- sum(diag(cm))/sum(cm))
(error <- 1 - accuracy)
(sensitivity <- cm[1,1]/(cm[1,1]+cm[2,1]))
(FNR <- 1 - sensitivity)
(specificity <- cm[2,2]/(cm[2,1]+cm[2,2]))
(FPR <- 1 - specificity)

# Confusion matrix and diagnostic measures from “caret” package:


library(caret)

predicted <-
  factor(ifelse(predict>0.5,1,0))
reference <- factor(data$Survived)
confusionMatrix(predicted, reference)


# Receiver Operating Characteristics (ROC) curve of full model:

library(ROCR)
ROCRpred <- prediction(predict,data$Survived)
ROCRperf <-performance(ROCRpred,'tpr','fpr')
plot(ROCRperf, colorize = TRUE,text.adj = c(-0.2,1.7))


#ROC curve to find Area Under Curve (AUC):

library(pROC)
predicted <-as.numeric(predicted)
roc1 <- roc(reference, predicted)
print(roc1)
plot(roc1)



#Translating the logistic model with data science approach:
  #Data partition
ind <- sample(2, nrow(data),
                  replace = T, prob = c(0.7, 0.3))
train <- data[ind==1,]
test <- data[ind==2,]

model.train <- glm(Survived ~.,
                     data=train, family = binomial)
summary(model.train)

#Confusion matrix of train data
predict.train <-
  predict(model.train,
          type="response")
predicted.train <-
  as.numeric(ifelse(predict.train>0.5,1,0))
(cm <-
     table(predicted.train,train$Survived))

#Confusion Matrix and diagnostic accuracy from caret package
predicted.train <-
  factor(ifelse(predict.train>0.5,1,0
  ))
reference.train <-
  factor(train$Survived)
confusionMatrix(predicted.train,
                  reference.train)

#ROC curve of “train” data:
#ROC Curve of Train data with ROCR package
 ROCRpred <-
  prediction(predict.train,
             test$Survived)
 ROCRperf <-
  performance(ROCRpred,
              'tpr','fpr')
 plot(ROCRperf, colorize = TRUE,
       text.adj = c(-0.2,1.7))
 
 predicted.train <-
   as.numeric(predicted.train)
 roc3 <- roc(reference.train,
               predicted.train)
 print(roc3)
 plot(roc3)
 
 
 #Prediction and accuracy for “test” data based on fitted model on the “train” data:
   #Prediction with fitted logistic regression model for train data
 predict.test <- predict(model.train,
                           test, type="response")
 predicted.test <-
   factor(ifelse(predict.test>0.5,1,0))
 reference.test <-
   factor(test$Survived)
 confusionMatrix(predicted.test,
                   reference.test)
 
 #ROC curve for “test” data”
 #ROCR Curve of Test data
 ROCRpred <-
   prediction(predict.test,
              test$Survived)
 ROCRperf <-
   performance(ROCRpred,
               'tpr','fpr')
 plot(ROCRperf, colorize = TRUE,
        text.adj = c(-0.2,1.7))
 
 #AUC for “test” data:
   #Use pROC package for AUC
library(pROC)
 predicted.test <-
   as.numeric(predicted.test)
 roc4 <- roc(reference.test,
               predicted.test)
 print(roc4)
 plot(roc4)
 
 
# ===========================Naive bayes ===============
library(e1071)
 # Setting Seed
set.seed(120)
 #Fitting model
 model.nb <- naiveBayes(Survived~.,
                          data=train)
 #Checking model
 model.nb
 
 
 #Prediction for the test data and confusion matrix:
 y_pred <- predict(model.nb,
                     newdata = test)
 # Confusion Matrix
 cm <- table(test$Survived,
               y_pred)
 cm
 
 library(caret)
# Model Evaluation
 confusionMatrix(cm)
 
 #==================SVM classifier-===========
 
 # Fitting SVM classifier on “titanic train” data: SVM has four “kernel”, we will use linear now!
   #Fitting the SVM classifier model
model.svm = svm(formula =Survived~.,
                data = train,
                type = 'C-classification',
                kernel = 'linear')
 #Check the model
 model.svm
 
 # Prediction for the test data and confusion  matrix:
   # Predicting on test data'
  y_pred.svm <-
   predict(model.svm, newdata =test)
 # Confusion Matrix
 cm.svm <- table(y_pred.svm,test$Survived)
 cm.svm
 
 confusionMatrix(cm.svm)
## ======================Decision Tree================
 
# Let’s fit a decision tree classifier in the Cardiotocographic (CTG) data:
   #Make sure the data is in the working library of R
 library(readr)
Cardiotocographic <- read_csv("Cardiotocographic.csv")
#Save the long name as data
 data <- Cardiotocographic
str(data)
#The structure correctly shows LB, AC and FM as numerical but incorrectly show NSP as numeric too!

#Change the NSP as factor variable as this is the dependent variable:
  #Three categories of NSP
  
#   NSP = 1 = Normal CTG (No
#                           hypoxia or acidosis)
# NSP = 2 = Suspicious CTG (Low
#                             probability of hypoxia/acidosis)
# NSP = 3 = Pathological CTG (High
#                               probability of hypoxia/acidosis)
#Changing as factor variable
data$NSPF <- factor(data$NSP)
str(data$NSPF)

#Let’s divide the data into train and test sets:
#Data partition
 set.seed(1234)
ind <- sample(2, nrow(data),
                replace=T, prob = c(0.8, 0.2))
train <- data[ind==1,]
test <- data[ind==2,]

library(party)

#Let’s fit a decision tree classifier in the train data:

#Fit the decision tree model with only the first three variable to learn it today
tree <- ctree(NSPF ~ LB+AC+FM,
                data=train)
plot(tree)


#Pruning the tree with 99% confidence interval and split at 500 samples
tree1 <- ctree(NSPF ~ LB+AC+FM,
                 data=train, controls =
                   ctree_control(mincriterion=0.99,
                                 minsplit=500))
plot(tree1)

# Let’s predict now:
  # Predict the categorical probabilities for each case in train data
predict(tree, type="prob")
#Predict the category for each case in test data
 predict(tree, test)
 
 #Confusion matrix
 (tab <- table(predict(tree),
                 train$NSPF))
 #Accuracy
 accuracy <-
   sum(diag(tab))/sum(tab)
 #Misclassification error
 mce <- 1 - accuracy
 
 #Bagging with ipred package
 #install.packages(“ipred”) if required
 library(ipred)
 MBTree <- bagging(NSP~., data = train, coob=T)
 print(MBTree)
 #Prediction
 MBPredict1 <- predict(MBTree, test)
 MBPredict1
 #Confusion matrix and accuracy
 confusionMatrix(MBPredict1, test$NSP)
 