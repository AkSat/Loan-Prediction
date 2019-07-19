library(dplyr)
library(car)
library(caret)
library(randomForest)
library(rpart)
library(rattle)
library(RColorBrewer)
library(mltools)
library(data.table)
library(ROCR)
library(irr)
library(dataPreparation)

setwd("F:/Data Contests/AnalyticsVidya/Completed/Loan Prediction")

train <- read.delim("train.txt",header = TRUE,sep = ",")
test <- read.delim("test.txt",header = TRUE,sep = ",")

summary(train)


test$Loan_Status <- NA


# combine the datasets
allD <- rbind(train,test)

summary(allD)

prop.table(table(train$Loan_Status))


allD$Loan_Status1 <- ifelse(allD$Loan_Status == "Y",1,
                            ifelse(allD$Loan_Status == "N",0,
                                   allD$Loan_Status))

names(allD)
allD <- allD[,-13]


# imputing values for Gender column =====================================

summary(allD$Gender)

allD$Gender[allD$Gender == ""] <- NA

allD$Gender1 <- ifelse(is.na(allD$Gender) == TRUE,"Male",
                        as.character(allD$Gender))
allD$Gender1 <- as.factor(allD$Gender1)

allD <- allD[,-2]


# imputing values for Married column =====================================

summary(allD$Married)

allD$Married[trimws(allD$Married) == ""] <- NA

t1 <- table(allD$Married,allD$Loan_Status1)

#no_rate <- t1[,1]/ rowSums(t1)
yes_rate <- t1[,2]/ rowSums(t1)

# allD %>% select(Married,Loan_Status) %>% filter(is.na(Married) == TRUE) %>%
#   group_by(Loan_Status) %>% summarize(n())

ind1 <- which(is.na(allD$Married))
allD$Loan_Status1[is.na(allD$Married) == TRUE]

# 3 yes divided by length(ind1) which is 3, hence rate is 1
# In that case, we take mode, i.e. "Yes"

allD$Married1 <- ifelse(is.na(allD$Married) == TRUE,"Yes",
                         as.character(allD$Married))

allD$Married1 <- as.factor(allD$Married1)

allD <- allD[,-2]


# imputing values for Dependents column =====================================

summary(allD$Dependents)

allD$Dependents[trimws(allD$Dependents) == ""] <- NA

t1 <- table(allD$Dependents,allD$Loan_Status)

norate <- t1[,1]/rowSums(t1)
yesrate <- t1[,2]/rowSums(t1)

# t2 <- allD %>% select(Dependents,Loan_Status) %>% 
#   filter(is.na(Dependents) == TRUE) %>%
#   group_by(Loan_Status) %>% summarize(Total = n())
# 
# class(t2)

index1 <- which(is.na(allD$Dependents))
table(allD$Loan_Status[index1])/length(index1)


summary(allD$Dependents)

allD$Dependents1 <- ifelse(is.na(allD$Dependents) == TRUE,"0",
                         as.character(allD$Dependents))

allD$Dependents1 <- as.factor(allD$Dependents1)

summary(allD$Dependents1)

names(allD)

allD <- allD[,-2]



# imputing values for Self-employed column =====================================

summary(allD$Self_Employed)

allD$Self_Employed[trimws(allD$Self_Employed) == ""] <- NA

# t1 <- table(allD$Self_Employed,allD$Loan_Status)
# 
# norate <- t1[,1]/rowSums(t1)
# yesrate <- t1[,2]/rowSums(t1)
# 
# 
# index1 <- which(is.na(allD$Self_Employed))
# table(allD$Loan_Status[index1])/length(index1)

allD$Self_Employed1 <- ifelse(is.na(allD$Self_Employed) == TRUE,"No",
                               as.character(allD$Self_Employed))

allD$Self_Employed1 <- as.factor(allD$Self_Employed1)

names(allD)

allD <- allD[,-3]


summary(allD)




# imputing values for LoanAmount column =====================================


summary(na.omit(allD$LoanAmount))

allD$LATiles <- ntile(allD$LoanAmount,10)

t1 <- table(allD$LATiles,allD$Loan_Status)

rate <- t1[,2]/rowSums(t1)

index1 <- which(is.na(allD$LoanAmount))
table(allD$Loan_Status[index1])/length(index1)
quantile(allD$LoanAmount,p=c(1:10)/10,na.rm = TRUE)


summary(na.omit(allD$LoanAmount))
summary(allD$LoanAmount)
allD$LoanAmount[which(is.na(allD$LoanAmount))] <- 142

summary(allD)



# imputing values for Loan_Amount_Term column ================================

summary(allD$Loan_Amount_Term)


unique(allD$Loan_Amount_Term)
ntile(allD$Loan_Amount_Term,10)

#table(allD$Loan_Amount_Term)

#t1 <- table(allD$Loan_Amount_Term,allD$Loan_Status)
t1 <- table(ntile(allD$Loan_Amount_Term,10),allD$Loan_Status)

rate <- t1[,2]/rowSums(t1)

index1 <- which(is.na(allD$Loan_Amount_Term))
table(allD$Loan_Status[index1])/length(index1)

quantile(allD$Loan_Amount_Term,p=c(1:10)/10,na.rm = TRUE)

allD$Loan_Amount_Term[index1] <- 360



summary(allD)



# imputing values for Credit_History column ================================

summary(allD$Credit_History)

unique(allD$Credit_History)

table(allD$Credit_History)

t1 <- table(allD$Credit_History,allD$Loan_Status)

rate <- t1[,2]/rowSums(t1)

index1 <- which(is.na(allD$Credit_History))
table(allD$Loan_Status[index1])/length(index1)

allD$Credit_History[index1] <- 1


summary(allD)


names(allD)
allD <- allD[,-14]


# check for outliers =====================================================

quantile(allD$ApplicantIncome,p=c(1:100)/100)
quantile(allD$ApplicantIncome,p=c(900:1000)/1000)


quantile(allD$CoapplicantIncome,p=c(1:100)/100)
quantile(allD$CoapplicantIncome,p=c(900:1000)/1000)


# ...... no outliers observed





# Derived Variables ==============================================

allD$Term_yrs <- ceiling(allD$Loan_Amount_Term / 12)

allD$TotalIncome <- allD$ApplicantIncome + allD$CoapplicantIncome

allD$LTR <- allD$LoanAmount / allD$Term_yrs

allD$ILR <- allD$TotalIncome / allD$LoanAmount

unique(allD$Term_yrs)

names(allD)
allD <- allD[,-c(3,4,6)]


# converting qualitative to quantitative =============================
names(allD)

str(allD)

with(allD,unique(Education))
allD$EducationLE <- ifelse(allD$Education == "Graduate",1,0)

with(allD,unique(Gender1))
allD$GenderLE <- ifelse(allD$Gender1 == "Male",1,0)


with(allD,unique(Married1))
allD$MarriedLE <- ifelse(allD$Married1 == "Yes",1,0)


with(allD,unique(Self_Employed1))
allD$Self_EmployedLE <- ifelse(allD$Self_Employed1 == "Yes",1,0)


names(allD)

allD <- allD[,-c(2,7,8,10)]


# binning the variables ===========================================

with(allD,unique(Term_yrs))

allD$Term_yrsBin <- ifelse(allD$Term_yrs <= 10,"Short",
                    ifelse(allD$Term_yrs > 10 & allD$Term_yrs <=20,"Intermediate",
                           "Long"))

allD$Term_yrsBin <- as.factor(allD$Term_yrsBin)


names(allD)
allD <- allD[,-7]



# One-hot encoding (dummy) ====================================

with(allD,unique(Dependents1))
with(allD,unique(Property_Area))
with(allD,unique(Term_yrsBin))

names(allD)
str(allD)
onehotData  <- allD[,c(4,6,14)]
onehotRes <- one_hot(as.data.table(onehotData))

allD <- cbind(allD[,-c(4,6,14)],onehotRes)




# scaling using min max to convert between 0 and 1 ==================

min1 <- min(allD$LoanAmount)
max1 <- max(allD$LoanAmount)
a1 <- (allD$LoanAmount - min1)/ (max1 - min1)
min(a1)
max(a1)
allD$LoanAmount <- a1


min1 <- min(allD$TotalIncome)
max1 <- max(allD$TotalIncome)
a1 <- (allD$TotalIncome - min1)/ (max1 - min1)
min(a1)
max(a1)
allD$TotalIncome <- a1


min1 <- min(allD$LTR)
max1 <- max(allD$LTR)
a1 <- (allD$LTR - min1)/ (max1 - min1)
min(a1)
max(a1)
allD$LTR <- a1


min1 <- min(allD$ILR)
max1 <- max(allD$ILR)
a1 <- (allD$ILR - min1)/ (max1 - min1)
min(a1)
max(a1)
allD$ILR <- a1





# separating train and test from allD ==============================
train1 <- allD[is.na(allD$Loan_Status) == FALSE,]
test1 <- allD[is.na(allD$Loan_Status) == TRUE,]



# Logistic Regression ===============================================

str(train1)

mod1 <- glm(data = train1[,-1],
            Loan_Status1 ~ .,family = "binomial")
summary(mod1)


mod3 <- glm(data = train1[,-1],Loan_Status1 ~ Credit_History +
              MarriedLE +
              Property_Area_Semiurban
            ,family = "binomial")
summary(mod3)


pred <- predict(mod3,type = "response")

prop.table(table(train1$Loan_Status1))

pred1 <- ifelse(pred >= 0.6872964,1,0)

kappa2(data.frame(pred1,train1$Loan_Status1))

confusionMatrix(table(pred1,train1$Loan_Status1),positive = "1")

predROC <- ROCR::prediction(pred,train1$Loan_Status1)
perf <- ROCR::performance(predROC,"tpr","fpr")

plot(perf)
abline(0,1)

auc <- ROCR::performance(predROC,"auc")
auc@y.values

# accuracy : 0.773



# Random forest =============================================

model_rfs <- caret::train(as.factor(Loan_Status1) ~ .,
                   data = train1[,-1], method = "rf")
model_rfs$results$Accuracy


# accuracy : 0.7979756 0.7801072 0.7711656
# Mean : 0.78


# Decision trees ========================================================

# train_dt <- allD[is.na(allD$Loan_Status) == FALSE,]
# train_dt$Loan_Status1  <-  ifelse(train_dt$Loan_Status == "Y",1,0)
# names(train_dt)
# train_dt <- train_dt[,-7]
# 
model_dt <- rpart(Loan_Status1~.,data = train1[,-1],
                  method = "class",
                  control = rpart.control(cp=0.01,minsplit = 2))

fancyRpartPlot(model_dt)

plotcp(model_dt)

r1 <- prune(model_dt,cp=0.13)

fancyRpartPlot(r1)

pred_dt <- predict(r1,type = "class")

levels(pred_dt)

pred_dt <- as.numeric(pred_dt)

pred_dt <- ifelse(pred_dt == 2,1,0)

prop.table(table(train1$Loan_Status1))

pred1 <- ifelse(pred >= 0.6872,1,0)

kappa2(data.frame(pred1,train1$Loan_Status1))

confusionMatrix(table(pred1,train1$Loan_Status1),positive = "1")

predROC_dt <- ROCR::prediction(pred1,train1$Loan_Status1)
perf_dt<- ROCR::performance(predROC_dt,"tpr","fpr")

plot(perf_dt)
abline(0,1)

auc_dt <- ROCR::performance(predROC_dt,"auc")
auc_dt@y.values


# accuracy : 0.7077656


predictionRF <- predict(model_rfs, newdata = test1)

prediction1 <- predict(mod3, newdata = test1)
prop.table(table(train1$Loan_Status1))
prediction_glm <- ifelse(prediction1 >= 0.6872964,1,0)

predictionRF1 <- ifelse(predictionRF == "0","N","Y")
solution_rf1 <- data.frame(Loan_ID = test1$Loan_ID, 
                          Loan_Status = predictionRF1)  
write.csv(solution_rf1,"submissions4_RF1.csv",row.names = FALSE)





prediction_glm1 <- ifelse(prediction_glm == 0,"N","Y")
solution_glm1 <- data.frame(Loan_ID = test1$Loan_ID, 
                           Loan_Status = prediction_glm1)  
write.csv(solution_glm1,"submissions4_glm1.csv",row.names = FALSE)




#Build model using knn
set.seed(7)
str(train1)
model_knn <-  caret::train(as.factor(Loan_Status1) ~ ., data = train1[,-1], 
                           method = "knn")
model_knn$results$Accuracy
# accuracy : 0.71

set.seed(7)
model_xgb <- caret::train(as.factor(Loan_Status1) ~ ., data = train1[,-1], 
                          method = "xgbTree")
model_xgb$results$Accuracy




#Build model using SVM
set.seed(7)
model_svm <- caret::train(as.factor(Loan_Status1) ~ ., data = train1[,-1], 
                          method = "svmRadial")
model_svm$results$Accuracy
# accuracy : [1] 0.7986041 0.8042382 0.8013987


#Build model using Avg Neural Network
set.seed(7)
model_avNNet <- caret::train(as.factor(Loan_Status1) ~ ., data = train1[,-1], 
                             method = "avNNet")
model_avNNet$results$Accuracy
# accuracy : [1] 0.7898701 0.7869881 0.7952739 0.7815318 0.7834158 0.7884226
# [7] 0.7788658 0.7742842 0.7780326
 


prediction_svm <- predict(model_svm, newdata = test1)
prediction_svm <- ifelse(prediction_svm == "0","N","Y")

prediction_avNNet <- predict(model_avNNet, newdata = test1)
prediction_avNNet <- ifelse(prediction_avNNet == "0","N","Y")

model_xgb$results

prediction_xgb <- predict(model_xgb, newdata = test1)
prediction_xgb <- ifelse(prediction_xgb == "0","N","Y")


#Prepare dataset as per Kaggle submission file
solution_svm <- data.frame(Loan_ID = test1$Loan_ID, 
                           Loan_Status = prediction_svm) 

solution_avNNet <- data.frame(Loan_ID = test1$Loan_ID, 
                              Loan_Status = prediction_avNNet) 

solution_xgb <- data.frame(Loan_ID = test1$Loan_ID, 
                           Loan_Status = prediction_xgb) 


write.csv(solution_svm,"submissions4_svm.csv",row.names = FALSE)
write.csv(solution_avNNet,"submissions4_avNNet.csv",row.names = FALSE)
write.csv(solution_xgb,"submissions4_xgb.csv",row.names = FALSE)


