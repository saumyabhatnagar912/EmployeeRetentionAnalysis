Emp_data_model <- read.csv("C:/Users/Ashok/Desktop/College/Data Science/Project/Employee_Dataset.csv")
Emp_data_model$left[Emp_data_model$left == 0] <- 'No'
Emp_data_model$left[Emp_data_model$left == 1] <- 'Yes'
prop.table(table(Emp_data_model$left))
summary(as.factor(Emp_data_model$left))
set.seed(10)
id = sample(2,nrow(Emp_data_model),prob=c(0.7,0.3),replace=TRUE)
training_data = Emp_data_model[id == 1,]
testing_data = Emp_data_model[id == 2,]
#############################################################
#################APPLYING CLASSIFICATION MODEL###############
#############################################################

###################NAIVE BAYES########################
install.packages("caret")
install.packages("e1071")
library(caret)
library(e1071)
str(training_data)
training_data$left <- as.factor(training_data$left)
model_nb = naiveBayes(left~.,data=training_data)
model_nb
head(training_data)
# Predictions using naive bayes
str(testing_data)
testing_data$left <- as.factor(testing_data$left)
predic_nb = predict(model_nb,newdata=testing_data,type="class")
predic_nb
# Checking accuracy using COnfusion Matrix
confusionMatrix(table(predic_nb,testing_data$left))
#predic_nb  
#     No  Yes
#No  2686  278
#Yes  709  806
#Accuracy : 0.7796          
summary(as.factor(testing_data$left))
#No  Yes 
#3395 1084 
summary(predic_nb)
#No  Yes 
#2964 1515 

#########################DECISION TREE#######################
install.packages("rpart")
install.packages("caret")
install.packages("e1071")
install.packages('rattle')
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(caret)
library(e1071)
model_tree = rpart(left~.,data=training_data)
# Decision Rules
print(model_tree)
prp(model_tree, cex = 0.56)
printcp(model_tree)
model_tree$cptable[which.min(model_tree$cptable[,"xerror"]),"CP"]
model_fit_2 = prune(model_tree,cp=0.01)
prp(model_fit_2, cex = 0.56)
# Predictions using tree
predic_tree = predict(model_fit_2,newdata = testing_data,type="class")
predic_tree
# Checking accuracy using COnfusion Matrix
confusionMatrix(table(predic_tree,testing_data$left))
#predic_tree   
#             No   Yes
#         No  3341   84
#       Yes   54   1000

#Accuracy : 0.9692  

summary(predic_tree)
summary(as.factor(testing_data$left))
# summary(predic_tree)
#No  Yes 
#3425 1054 
# summary(as.factor(testing_data$left))
#No  Yes 
#3395 1084 
#Accuracy : 0.9692 

######################RANDOM FOREST#####################
install.packages('randomForest')
library(randomForest)
class(training_data$left)
training_data$left <- factor(training_data$left)
model_forest = randomForest(left~.,data=training_data)
summary(model_forest)
model_forest
#Dotchart of variable importance as measured by a Random Forest
varImpPlot(model_forest, main = "Importance")
getTree(model_forest)
predic_forest = predict(model_forest,newdata=testing_data,type="class")
predic_forest
# Checking accuracy using COnfusion Matrix

confusionMatrix(table(predic_forest,testing_data$left))
#predic_forest   
#     No  Yes
#No  3387   37
#Yes    8 1047


#Accuracy : 0.99    
summary(predic_forest)
#No  Yes 
#3424 1055
summary(as.factor(testing_data$left))
#No  Yes 
#3395 1084 
