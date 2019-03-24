
setwd("C:/Users/Ashok/Desktop/College/Data Science/Project")
gc()
rm(list = ls())
getwd()

Emp_data <- read.csv("C:/Users/Ashok/Desktop/College/Data Science/Project/Employee_Dataset.csv")
View(Emp_data)
str(Emp_data)
attach(Emp_data)
summary(as.factor(left))
#Employee status: Not left/Left
#0(Not left)      1(left)
#11428          3571 
Emp_data$leftnew <- 'Yes'
Emp_data$leftnew[Emp_data$left == 0] <- 'No'

################################################################
######################DATA VISUALIZATION########################
################################################################

par(mfrow=c(1,2))

######DEPARTMENT WISE DISTRIBUTION AND ATTRITION################

par(mar = c(2,2,2,2))
deptdata <- Emp_data$department
deptdatatable <- table(deptdata)
percent<- round(deptdatatable*100/length(deptdata))
lbls <- paste(names(deptdatatable), "\n", percent, "%", sep="")
pie(deptdatatable, labels = lbls, col = rainbow(10),
    main="Department wise Employee distribution (Total)")
box()
#accounting          hr          IT  management   marketing product_mng       RandD 
#767         739        1227         630         858         902         787 
#sales     support   technical 
#4140        2229        2720 
deptleft <- Emp_data$department[left == 1]
deptlefttable <- table(deptleft)
percentleft<- round(deptlefttable*100/length(deptleft))
lbls <- paste(names(deptlefttable), "\n", percentleft, "%", sep="")
pie(deptlefttable, labels = lbls, col = rainbow(10),
    main="Department wise Employee attrition")
box()
#accounting          hr          IT  management   marketing product_mng       RandD 
#204         215         273          91         203         198         121 
#sales     support   technical 
#1014         555         697 

#####################EMPLOYEE SATISFACTION########################
Emp_data$satisfaction <- '0.80+'
Emp_data$satisfaction[Emp_data$satisfaction_level >= 0.00 & Emp_data$satisfaction_level <= 0.40] <- '0.00-0.40'
Emp_data$satisfaction[Emp_data$satisfaction_level >= 0.41 & Emp_data$satisfaction_level <= 0.80] <- '0.41-0.80'

hist(Emp_data$satisfaction_level, freq = T, xlim = c(0,1), border = "black",
     include.lowest = T, main = "Satisfaction levels",
     xlab = "Satisfaction rating from 0 to 1", col = "Blue", 
     density = 10, ylim = NULL)
box()
satisfactionplot <- prop.table(table(Emp_data$leftnew,Emp_data$satisfaction),2)*100
barplot(satisfactionplot, main = "Satisfaction level vs Left", ylab="left %", 
        xlab = "satisfaction level", names.arg = c("0.00-0.40","0.41-0.80","0.80+"),
        ylim = c(0,100), xlim = c(0,18), beside = TRUE, axis.lty = 1, col = c("green","red"), 
        xpd = F, legend.text = T, cex.names = 0.75)
box()
#     0.00-0.40 0.41-0.80    0.80+
#No   44.46223  83.65299 86.28186
#Yes  55.53777  16.34701 13.71814



#####################LAST EVALUATION#############################
Emp_data$eval <- '0.85+'
Emp_data$eval[Emp_data$last_evaluation >= 0.30 & Emp_data$last_evaluation <= 0.45] <- '0.30-0.45'
Emp_data$eval[Emp_data$last_evaluation >= 0.46 & Emp_data$last_evaluation <= 0.60] <- '0.46-0.60'
Emp_data$eval[Emp_data$last_evaluation >= 0.61 & Emp_data$last_evaluation <= 0.85] <- '0.61-0.85'

hist(Emp_data$last_evaluation, freq = T, xlim = c(0,1),ylim = c(0,2500), border = "black",
     include.lowest = T, main = "Evaluation ratings",
     xlab = "Evaluation rating from 0 to 1", col = "Blue", 
     density = 10)
box()
evaluationplot <- prop.table(table(Emp_data$leftnew,Emp_data$eval),2)*100
barplot(evaluationplot, main = "Evaluation rating vs Left", ylab="Left %", 
        xlab = "Evaluation rating", names.arg = c("0.30-0.45","0.46-0.60","0.61-0.85","0.80+"),
        ylim = c(0,100), xlim = c(0,18), beside = TRUE, axis.lty = 1, legend.text = TRUE,
        col = c("green","red"))
box()
#     0.30-0.45  0.46-0.60  0.61-0.85 0.85+
#No   88.39286   64.79673   89.06725  68.21272
#Yes  11.60714   35.20327   10.93275  31.78728


#########################WORK LOAD#############################
Emp_data$projects <- '6+'
Emp_data$projects[Emp_data$number_project == 2 | Emp_data$number_project == 3] <- '2-3'
Emp_data$projects[Emp_data$number_project == 4 | Emp_data$number_project == 5] <- '4-5'

hist(Emp_data$number_project, freq = T, xlim = c(0,10),ylim = NULL, border = "black",
     include.lowest = T, main = "Projects distribution",
     xlab = "Number of projects", col = "Blue", 
     density = 10)
box()
projectsplot <- prop.table(table(Emp_data$leftnew,Emp_data$projects),2)*100
barplot(projectsplot, main = "Number of projects vs Left", 
        ylab="Left %", 
        xlab = "Projects", names.arg = c("2-3","4-5","6+"),
        ylim = c(0,100), xlim = c(0,15), beside = TRUE, axis.lty = 1, legend.text = TRUE,
        col = c("green","red"))
box()
#       2-3      4-5       6+
#No  74.56154 85.67219 36.29371
#Yes 25.43846 14.32781 63.70629

#########################AVERAGE MONTHLY HOURS#############################
Emp_data$hours <- '251+'
Emp_data$hours[Emp_data$average_montly_hours >= 96 & Emp_data$average_montly_hours <= 150] <- '96-150'
Emp_data$hours[Emp_data$average_montly_hours >= 151 & Emp_data$average_montly_hours <= 250] <- '151-250'

hist(Emp_data$average_montly_hours, freq = T, xlim = c(0,350),ylim = NULL, border = "black",
     include.lowest = T, main = "Monthly hours",
     xlab = "Average monthly hours", col = "Blue", 
     density = 10,)
box()
monthlyhoursplot <- prop.table(table(Emp_data$leftnew,Emp_data$hours),2)*100
barplot(monthlyhoursplot, main = "Average monthly hours vs Left", 
        ylab="Left %", 
        xlab = "Average hours spent", names.arg = c("151-250","250+","90-150"),
        ylim = c(0,100), xlim = c(0,15), beside = TRUE, axis.lty = 1, legend.text = TRUE,
        col = c("green","red"))
box()
#     151-250     250+   90-150
#No  85.78454 61.21174 64.42843
#Yes 14.21546 38.78826 35.57157


############################EXPERIENCE#############################
years <- '6+'
Emp_data$years <- '6+'
Emp_data$years[Emp_data$time_spend_company == 2 | Emp_data$time_spend_company == 3] <- '2-3'
Emp_data$years[Emp_data$time_spend_company == 4 | Emp_data$time_spend_company == 5] <- '4-5'

hist(Emp_data$time_spend_company, freq = T, xlim = c(0,10),ylim = NULL, border = "black",
     include.lowest = T, main = "Experience in Years",
     xlab = "Years", col = "Blue", 
     density = 10)
box()
experienceplot <- prop.table(table(Emp_data$leftnew,Emp_data$years),2)*100
barplot(experienceplot, main = "Experience vs Left", 
        ylab="Left %", 
        xlab = "Years", names.arg = c("2-3","4-5","6+"),
        ylim = c(0,100), xlim = c(0,18), beside = TRUE, axis.lty = 1, legend.text = TRUE,
        col = c("green","red"))
box()

#        2-3      4-5       6+
#  No  83.08042 57.24566 83.69735
#Yes 16.91958 42.75434 16.30265

############################WORK ACCIDENTS###########################
wan<-length(Emp_data$Work_accident[Emp_data$Work_accident == 0])
way<-length(Emp_data$Work_accident[Emp_data$Work_accident == 1])
wa<-cbind(wan,way)

barplot(as.matrix(wa), width = 10, main = "Work accidents", ylab = "Numbers", xlab = "Accident occured",
        beside = T, names.arg = c("No","Yes"), ylim = c(0,15000), 
        col = NULL, density = c(10,10),
        legend.text = TRUE, xlim = c(0,130))
box()
accidentplot <- prop.table(table(Emp_data$leftnew,Emp_data$Work_accident),2)*100

barplot(accidentplot, main = "Work accident vs Left", 
        ylab="Left %", 
        xlab = "Accident occured", names.arg = c("No","Yes"),
        ylim = c(0,100), xlim = c(0,15), beside = TRUE, axis.lty = 1, legend.text = TRUE,
        col = c("green","red"))
box()
#       (Accidents)
#      No         Yes
#No  73.484022 92.208391
#Yes 26.515978  7.791609


############################LAST PROMOTION###########################
pn<-length(Emp_data$promotion_last_5years[Emp_data$promotion_last_5years == 0])
py<-length(Emp_data$promotion_last_5years[Emp_data$promotion_last_5years == 1])
p<-cbind(pn,py)

barplot(as.matrix(p), width = 10, main = "Promotions in 5 Years", ylab = "Frequency", 
        xlab = "Promotion received",
        beside = T, names.arg = c("No","Yes"), ylim = c(0,16000), 
        col = NULL, density = c(10,10),
        legend.text = TRUE, xlim = c(0,130))
box()
promotionplot <- prop.table(table(Emp_data$leftnew,Emp_data$promotion_last_5years),2)*100

barplot(accidentplot, main = "Promotion vs Left", 
        ylab="Left %", 
        xlab = "Promotion received", names.arg = c("No","Yes"),
        ylim = c(0,100), xlim = c(0,15), beside = TRUE, axis.lty = 1, legend.text = TRUE,
        col = c("green","red"))
box()

# (Promotion in 5 Years)
#       No         Yes
#No  75.803815 94.043887
#Yes 24.196185  5.956113

############################SALARY###########################
Emp_data$salarylevel <- 1
Emp_data$salarylevel[Emp_data$salary == 'medium'] <- 2
Emp_data$salarylevel[Emp_data$salary == 'high'] <- 3
View(Emp_data)
head(Emp_data)
Emp_data_num <- Emp_data[,c(1,2,3,4,5,6,8,17,7)]
par(mar=c(2,2,2,2))
hist(Emp_data_num$salarylevel, freq = T, xlim = c(0,3),ylim = c(0,8000), 
     border = "black",
     include.lowest = T, main = "Salary Level",
     xlab = "Salary", col = "Blue", 
     density = 10)
box()
count <- prop.table(table(Emp_data$leftnew,Emp_data_num$salarylevel),2)
barplot(count, main = "Salary level vs Left", ylab="left %", 
        xlab = "salary level", names.arg = c("low","medium","high"),
        ylim = c(0,1), beside = TRUE, axis.lty = 1, legend.text = TRUE,
        col = c("green","red"), xlim = c(0,18))
box()

#       Low       Medium      High
#No  0.70311646 0.79568725 0.93371059
#Yes 0.29688354 0.20431275 0.06628941


###########################################################
###########CORRELATION OF ATTRIBUTES(Numeric)##############
###########################################################
install.packages("corrplot")
library(corrplot)
head(Emp_data_num)
cr<- cor(Emp_data_num)
cr
par(mfrow=c(1,1))
par(mar=c(2,2,2,2))
corrplot(cr, type = "lower", method = "pie", addCoef.col = "black", 
         addgrid.col = "black")
cr


###########################################################
###########CORRELATION OF ATTRIBUTES(Categorical)##########
###########################################################
##chisquared test fr p-value of categorical variables

chisq.test(Emp_data$leftnew,Emp_data$department, correct = TRUE, 
           p = rep(1/length(x),length(x)),rescale.p = FALSE,
           simulate.p.value = FALSE, B = 2000)
#Pearson's Chi-squared test
#data:  Emp_data$leftnew and Emp_data$department
#X-squared = 86.825, df = 9, p-value = 7.042e-15

chisq.test(Emp_data$leftnew,Emp_data$salary, correct = TRUE, 
           p = rep(1/length(x),length(x)),rescale.p = FALSE,
           simulate.p.value = FALSE, B = 2000)
#Pearson's Chi-squared test
#data:  Emp_data$leftnew and Emp_data$salary
#X-squared = 381.23, df = 2, p-value < 2.2e-16

chisq.test(Emp_data$left,Emp_data$satisfaction_level, correct = TRUE, 
           p = rep(1/length(x),length(x)),rescale.p = FALSE,
           simulate.p.value = FALSE, B = 2000)
#Pearson's Chi-squared test
#data:  Emp_data$left and Emp_data$satisfaction_level
#X-squared = 7937.7, df = 91, p-value < 2.2e-16

#############################END###########################
aggregate(left~department,
          data = Emp_data, FUN = function(x) {sum(x)/length(x)})
