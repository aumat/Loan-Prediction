getwd()
train<- read.csv("train.csv",na.strings = c(""," ","NA"))
test<- read.csv("test.csv",na.strings = c(""," ","NA"))
summary(train)

colSums(is.na(train))

train$Credit_History<- ifelse(is.na(train$Credit_History),"Missing",as.character(train$Credit_History))
train$Credit_History<- as.factor(train$Credit_History)
train$appx_emi= train$LoanAmount/train$Loan_Amount_Term
train$hhold_income=(train$ApplicantIncome+train$CoapplicantIncome)
train$Credit_History <-as.factor(train$Credit_History)
train<- train[-c(7:8)]
train<-na.omit(train)
summary(train)
summary(test)


#Missing data imputation on test data

library(dplyr)
library(tidyr)

Missing<-test[!complete.cases(test),]

test<-test %>% replace_na(list(Gender="Male", Self_Employed="No",Dependents="0"))

test$Credit_History<- ifelse(is.na(test$Credit_History),"Missing",as.character(test$Credit_History))
test$Credit_History<- as.factor(test$Credit_History)
summary(test$Credit_History)


test$Loan_Amount_Term<-ifelse(is.na(test$Loan_Amount_Term),360,test$Loan_Amount_Term)


test$LoanAmount<- ifelse(is.na(test$LoanAmount),128,test$LoanAmount)

test$appx_emi= test$LoanAmount/test$Loan_Amount_Term
test$hhold_income=(test$ApplicantIncome+test$CoapplicantIncome)

colSums(is.na(test))

test<-test[-c(7:8)]
summary(test)

library(ggplot2)
#Target Variable is Loan Status i.e. Loan approved 1=Yes/ 0=No

#Data Exploration

#Loan Status Vs Gender
P1<- ggplot(train,aes(x=Gender,fill=Loan_Status))
P1+geom_bar()+ggtitle("Loan Status Vs Gender")+theme(plot.title = element_text(hjust = 0.5))

#Loan Status Vs Marriage status

P2<- ggplot(train,aes(x=Married,fill=Loan_Status))
P2+geom_bar()+ggtitle("Loan Status Vs Marriage Status")+theme(plot.title = element_text(hjust = 0.5))

#Clearly Married have more loan approval status than which is intutively true

#Loan Status Vs No of Dependents
P3<- ggplot(train,aes(x=Loan_Status,fill= Dependents))
P3+geom_bar()+ggtitle("Loan Status Vs No of Dependents")+theme(plot.title = element_text(hjust = 0.5))

# The more the dependents, the more the expenditure and lesser the ability to repay the loan.
# Loan status approval is higher for customers with lesser no of dependents

# Loan Status Vs Education
P4<- ggplot(train,aes(x=Loan_Status,fill=Education))
P4+geom_bar()+ggtitle("Loan Status Vs Education")+theme(plot.title = element_text(hjust = 0.5))

# The customers with Graduation have higher loan approvals


# Loan Status Vs Self_Employed

P5<- ggplot(train,aes(x=Loan_Status,fill=Self_Employed))
P5+geom_bar()+ggtitle("Loan Status Vs Self_Employed")+theme(plot.title = element_text(hjust = 0.5))

# Loan approvals is high among non-self employed customers

# Loan Status Vs Household Income

P6<- ggplot(train,aes(fill=Loan_Status,x= hhold_income))
P6+geom_histogram(breaks=seq(0,81000,by=5000))+ggtitle("Loan Status Vs Household Income")+theme(plot.title = element_text(hjust = 0.5))

P7<- ggplot(train,aes(x=Loan_Status,y= hhold_income))
P7+geom_boxplot()+ggtitle("Loan Status Vs Household Income")+theme(plot.title = element_text(hjust = 0.5))

# Removing hhold_income=81000 for loan Status 'N' 

train<- train %>% filter(hhold_income!=81000)

#Loan Status Vs  LoanAmount

# P8<- ggplot(train,aes(x=Loan_Status,y=  LoanAmount))
P8+geom_boxplot()+ggtitle("Loan Status Vs  LoanAmount")+theme(plot.title = element_text(hjust = 0.5))

P9<- ggplot(train,aes(x=  LoanAmount, fill=Loan_Status))
P9+geom_histogram(breaks=seq(0,600,100))+ggtitle("Loan Status Vs  LoanAmount")+theme(plot.title = element_text(hjust = 0.5))


P10<- ggplot(train,aes(fill=Loan_Status,x= Credit_History))
P10+geom_bar()+ggtitle("Loan Status Vs  Credit_History")+theme(plot.title = element_text(hjust = 0.5))


P11<- ggplot(train,aes(x=Loan_Status,fill= Credit_History, y=hhold_income))
P11+geom_boxplot()+ggtitle("Loan Status Vs  Credit_History")+theme(plot.title = element_text(hjust = 0.5))

P12<- ggplot(train,aes(fill=Self_Employed,x= Credit_History))
P12+geom_bar()+ggtitle("EMPLOYMENT STATUS Vs  Credit_History")+theme(plot.title = element_text(hjust = 0.5))

train %>% filter(hhold_income>35000 & Credit_History==0)

# Logistic regression

library(caret)
library(ROCR)

logr<- glm(Loan_Status~.,data=train[,-1],family = "binomial")
summary(logr)

step(logr)

logm<-glm(formula = Loan_Status ~ Married  + Credit_History + Property_Area + appx_emi, family = "binomial", data = train[,-1])

summary(logm)

pred<- predict(logm, newdata=test, type="response")

pred<- ifelse(pred>0.5,"Y","N")

soln<-data.frame(Loan_ID=test$Loan_ID,Loan_Status=pred)

write.csv(soln,file="sol.csv",row.names = F)



#Score 78.4

# Using xgboost

library(xgboost)
train_xg <-train[-c(1,11)]
train_xg<-model.matrix(~. ,data=train_xg)
label_xg<- as.factor(train$Loan_Status)
label_xg<- ifelse(train$Loan_Status=="Y",1,0)
head(label_xg)
test_xg <-test[-c(1)]
test_xg <- model.matrix(~.,data=test_xg)

params=list()
params$objective <- "binary:logistic"
params$eta <- 0.2
params$max_depth <- 5
params$subsample <- 0.5
params$colsample_bytree <- 0.5
params$min_child_weight <- 2
params$eval_metric <- "auc"

xgb_cv <- xgb.cv(data= train_xg,label = label_xg,params=params, nrounds = 1000, nfold = 5, early.stop.round = 30, prediction = TRUE,set.seed(345))


min.loss.idx = which.min(xgb_cv$dt[, test.logloss.mean]) 
cat ("Minimum logloss occurred in round : ", min.loss.idx, "\n")

#xg boost
params=list()
params$objective <- "binary:logistic"
params$eta <- 0.2
params$max_depth <- 5
params$subsample <- 0.5
params$colsample_bytree <- 0.5
params$min_child_weight <- 2
params$eval_metric <- "auc"

xgb_m <-xgboost(data=train_xg,label=label_xg,params=params,nrounds=22)

pred<-predict(xgb_m,test_xg)

#Uploading solution

sol_xgb<- data.frame(Loan_ID=test$Loan_ID,Loan_Status=pred)
sol_xgb$Loan_Status<- ifelse(sol_xgb$Loan_Status>0.25,"Y","N")
colSums(is.na(sol_xgb))
head(sol_xgb)

table(train$Loan_Status)/nrow(train)



write.csv(sol_xgb,file="sol_xgb.csv",row.names = F)


#Random forest

library(randomForest)
set.seed(111)
rf<- randomForest(Loan_Status ~ Credit_History + hhold_income+ LoanAmount+appx_emi  ,data=train[,-1], importance=T,ntree=500,mtry=3)
summary(rf)
round(importance(rf), 2)
pdct<- predict(rf, newdata=test[,-1], type="class")
sol_rf<- data.frame(Loan_ID=test$Loan_ID,Loan_Status=pdct)
write.csv(sol_rf,file="sol_rf.csv",row.names = F)

#Decision trees

library(rpart)
library(rattle)
set.seed(321)
dt<- rpart(Loan_Status ~  Credit_History + hhold_income ,data=train[,-1],control=rpart.control(minbucket = 5,cp=0.00001,maxdepth = 5))
fancyRpartPlot(dt)
prdct<- predict(dt, newdata=test[,-1], type="class")
sol_dt<- data.frame(Loan_ID=test$Loan_ID,Loan_Status=pdct)
write.csv(sol_dt,file="sol_dt.csv",row.names = F)
