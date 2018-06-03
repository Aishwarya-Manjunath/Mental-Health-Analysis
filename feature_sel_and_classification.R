df<-read.csv('survey.csv',stringsAsFactors = FALSE)
#to check if country has NA
any(is.na(df$Country))
#plotting time series data

age1<-c()
ct<-1
#below for loop finds all the age values which are within 15 and 60
for(i in 1:nrow(df)){
  if(df$Age[i]>=15 || df$Age[i]<=60){
    #age1 contains these age values b/w 15 & 60
    age1[ct]<-df$Age[i]
    ct<-ct+1
  }
}
median_age1<-median(age1)
#the below for loop replaces the age values <15 & >60 with the median value
for(i in 1:nrow(df)){
  if(df$Age[i]<15 || df$Age[i]>60){
    df$Age[i]<-median_age1
  }
}

unique(df$Gender)
# dealing with gender by replacing inconsistent values of gender with consistent ones
g<-c()
for(i in 1:nrow(df)){
  x<-df$Gender[i]
  if(x=="fluid"){
    g[i]<-"Other"
  }
  else if("Trans"==substr(x,1,5) || "trans"==substr(x,1,5) || regexpr("trans",x)[1]!=-1 || regexpr("Trans",x)[1]!=-1){
    if(substr(x,7,7)=="f" || substr(x,7,7)=="w"){
      g[i]<-"Trans-Female"
    }
    else if(substr(x,1,1)=="F" || substr(x,1,1)=="f"){
      g[i]<-"Trans-Female"
    }
  }
  else if("M"==substr(x,1,1) || "m"==substr(x,1,1) || "Guy"==substr(x,1,3) || regexpr("male",x)[1]!=-1){
    g[i]<-"Male"
  }
  else if("F"==substr(x,1,1) || "f"==substr(x,1,1) || "w"==substr(x,1,1) || "W"==substr(x,1,1)){
    g[i]<-"Female"
  }
  else if("Cis"==substr(x,1,3) || "cis"==substr(x,1,3)){
    if(substr(x,5,5)=="f" || substr(x,5,5)=="F"){
      g[i]<-"Female"
    }
    else if(substr(x,5,5)=="m" || substr(x,5,5)=="M"){
      g[i]<-"Male"
    }
  }
  else
    g[i]<-"Other"
}
#replacing gender
df$Gender<-g

#drop column comments
df<-subset(df,select=-comments)

#drop column no_employees
df<-subset(df,select=-no_employees)

#drop rows with NA for self_employed
df<-df[-c(1:18),]

#NA here are replaced by a new category - Not Known
df[is.na(df$work_interfere),"work_interfere"] <- "Not Known"


df_new <- df

cname <- names(df)
cname <- setdiff(cname,c("Timestamp","state","Country"))
cname1<-cname

#binary encoding
convert_to_binary<-function(p_number) {   #function to convert from decimal to binary
  bsum<-0
  bexp<-1
  while (p_number > 0) {
    digit<-p_number %% 2
    p_number<-floor(p_number / 2)
    bsum<-bsum + digit * bexp
    bexp<-bexp * 10
  }
  return(bsum)
}
df_new <- data.frame(sl_no=c(1:nrow(df)))  #initializee new data frame

cname<-setdiff(cname,c("Age","treatment")) #attributes to binary encode

for(j in 1:length(cname))
{     value<-c()
      var<-unique(df[,cname[j]])
      for(i in 0:(length(var)-1))
      {
             value<-c(value,toString(convert_to_binary(as.integer(i))))
      }
      #value stores binary form of all ordinal values of categories
      max_new_col <- nchar(value[length(value)])

      for(i in 1:length(value))
      {
          value[i]<-paste0(strrep("0",max_new_col-nchar(value[i])),value[i])
      }

      #each attribute is split into max_new_col number of attributes
      bin_enc_mat<- matrix(nrow=nrow(df),ncol = max_new_col) 

      for(i in 1:nrow(df))
      {   x <- strsplit(value[match(df[i,cname[j]],var)],'')
          for(k in 1:max_new_col)
          { 
               bin_enc_mat[i,k]<- as.integer(x[[1]][k])
          }  
      }
      #bin_enc_mat contains the new form of an attribute on every iteration
      for(i in 1:max_new_col)   #append new columns to new data frame
      {
            df_new<-cbind(df_new,x=bin_enc_mat[,i])
            names(df_new)[names(df_new)=="x"]<-paste0(paste0(cname[j],"_"),i)
      }
}

#append Age and treatment to new data frame
df_new<-cbind(df_new,Age=df$Age)
df_new<-cbind(df_new,treatment=df$treatment)
cname<-c(cname,"treatment")

#convert into categorical again
for(j in 1:length(cname))
{
  df[,cname[j]]<-as.factor(df[,cname[j]])
}
cname<-c(cname,"Age")


library(caret)

set.seed(2)
#divide the datset into train and test in the ratio 70:30
trainInp <- createDataPartition(y = df$treatment, p= 0.7, list = FALSE)
trainData <- df[trainInp,]
testData <- df[-trainInp,]

my_treatment<-df$treatment
df<-df[,setdiff(cname,c("treatment"))]
  
#feature selection using random forest
library(randomForest)

#building a random forest model
fit <- randomForest(as.factor(trainData$treatment)~.,data=trainData[,cname])
#to find the the most important attributes from the model
importanceOrder <- order(-fit$importance)
signif_rf <- rownames(fit$importance)[importanceOrder][1:10]
#plotting importance of all variables
varImpPlot(fit,type=2, main = "Variable Importance using Random Forest",xlab="Attributes")
Sys.sleep(1)

#feature selection using boruta algorithm
library(Boruta)
# Decide if a variable is important or not using Boruta
boruta_output <- Boruta(my_treatment~ ., data=na.omit(df), doTrace=2)  # perform Boruta search
#obtaining the significant variable names
boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed")])  # collect Confirmed and Tentative variables
print(boruta_signif)  # print significant variables
#plotting importance of all variables
plot(boruta_output, cex.axis=.6, las=3, xlab="", main="Variable Importance using Boruta")  # plot variable importance
boruta.df <- attStats(boruta_output)
Sys.sleep(1)

#load package for feature selection with chisquare and information gain
library(FSelector)
#feature selection using chi-square test
weights1<- chi.squared(my_treatment~.,data=na.omit(df))
print(weights1)
#obtaining the 10 most important features
subset_chisq <- cutoff.k(weights1, 10)
f <- as.simple.formula(subset_chisq, "my_treatment")
print(f)
#plotting importance of all variables
plot(weights1$attr_importance,xaxt='n', ylab="Importance",xlab="", main="Variable Importance using Chi-square Test")
axis(1,at=1:21,labels=names(df),cex.axis=.57, las=3) 
Sys.sleep(1)


#feature selection using information gain
weights2 <- information.gain(my_treatment~., data=na.omit(df))
print(weights2)
#obtaining the 10 most important features
subset_ifg <- cutoff.k(weights2, 10)
f <- as.simple.formula(subset_ifg, "my_treatment")
print(f)
#plotting importance of all variables
plot(weights2$attr_importance,xaxt='n', xlab="",ylab="Importance", main="Variable Importance using Mutual Information Gain")
axis(1,at=1:21,labels=names(df),cex.axis=.4, las=3) 
Sys.sleep(1)

#function to convert text categories to binary number {0,1}
convert_to_number<-function(pred)
{   pred_n<-c()
    for(i in 1:length(pred))
    {
       if(pred[i]=="Yes")
             pred_n<-c(pred_n,1)
       else
             pred_n<-c(pred_n,0)
    }
    return(pred_n)
} 

#Random Forest classifier
#prediction using the random forest classifier
pred_rf<-predict(fit,testData[,setdiff(cname,c("treatment"))])
t<-table(predict=pred_rf,truth=testData$treatment)
#constructing a confusion matrix of the result
confusionMatrix(t,positive = "Yes")
pred_rf_n <- convert_to_number(pred_rf)

#random forest along with the features selected from random forest
rf_rf <- randomForest(as.factor(trainData$treatment)~.,data=trainData[,signif_rf])
pred_rf_rf<-predict(rf_rf,testData[,signif_rf])
t<-table(predict=pred_rf_rf,truth=testData$treatment)
confusionMatrix(t,positive = "Yes")
pred_rf_rf_n <- convert_to_number(pred_rf_rf)

#random forest along with the features selected from boruta
rf_brt <- randomForest(as.factor(trainData$treatment)~.,data=trainData[,boruta_signif])
pred_rf_brt<-predict(rf_brt,testData[,boruta_signif])
t<-table(predict=pred_rf_brt,truth=testData$treatment)
confusionMatrix(t,positive = "Yes")
pred_rf_brt_n <- convert_to_number(pred_rf_brt)

#random forests along with chi-square test
rf_chsq <- randomForest(as.factor(trainData$treatment)~.,data=trainData[,subset_chisq])
pred_rf_chsq<-predict(rf_chsq,testData[,subset_chisq])
t<-table(predict=pred_rf_chsq,truth=testData$treatment)
confusionMatrix(t,positive = "Yes")
pred_rf_chsq_n <- convert_to_number(pred_rf_chsq)

#random forest along with information gain test
rf_ifg <- randomForest(as.factor(trainData$treatment)~.,data=trainData[,subset_ifg])
pred_rf_ifg<-predict(rf_ifg,testData[,subset_ifg])
t<-table(predict=pred_rf_ifg,truth=testData$treatment)
confusionMatrix(t,positive = "Yes")
pred_rf_ifg_n <- convert_to_number(pred_rf_ifg)

#naive bayes classifier
library(e1071)
#building naive bayes model
model <- naiveBayes(treatment~., data = trainData[,cname])
class(model) 
#prediction using test data with naive bayes model
pred_nb <- predict(model,testData[,setdiff(cname,"treatment")])
t_nb<-table(predict=pred_nb,truth=testData$treatment)
#constructing a confusion matrix of the result
confusionMatrix(t_nb,positive = "Yes")
pred_nb_n <- convert_to_number(pred_nb)

#naive bayes along with the features selected from random forest
model_nb_rf<- naiveBayes(trainData$treatment~., data = trainData[,signif_rf])
class(model_nb_rf) 
pred_nb_rf <- predict(model_nb_rf,testData[,signif_rf])
t_nb_rf<-table(predict=pred_nb_rf,truth=testData$treatment)
confusionMatrix(t_nb_rf,positive = "Yes")
pred_nb_rf_n <- convert_to_number(pred_nb_rf)

#naive bayes along with the features selected from boruta
model_nb_boruta <- naiveBayes(trainData$treatment~., data = trainData[,boruta_signif])
class(model_nb_boruta) 
pred_nb_boruta <- predict(model_nb_boruta,testData[,boruta_signif])
t_nb_boruta<-table(predict=pred_nb_boruta,truth=testData$treatment)
confusionMatrix(t_nb_boruta,positive = "Yes")
pred_nb_brt_n <- convert_to_number(pred_nb_boruta)

#naive bayes along with chi-square test
model_nb_chisq <- naiveBayes(trainData$treatment~., data = trainData[,subset_chisq])
class(model_nb_chisq) 
pred_nb_chisq <- predict(model_nb_chisq,testData[,subset_chisq])
t_nb_chisq<-table(predict=pred_nb_chisq,truth=testData$treatment)
confusionMatrix(t_nb_chisq,positive = "Yes")
pred_nb_chsq_n <- convert_to_number(pred_nb_chisq)

#naive bayes along with information gain test
model_nb_ifg <- naiveBayes(trainData$treatment~., data = trainData[,subset_ifg])
class(model_nb_ifg) 
pred_nb_ifg <- predict(model_nb_ifg,testData[,subset_ifg])
t_nb_ifg<-table(predict=pred_nb_ifg,truth=testData$treatment)
confusionMatrix(t_nb_ifg,positive = "Yes")
pred_nb_ifg_n <- convert_to_number(pred_nb_ifg)

#logistic regression
#building a logistic regression model 
reg = glm(treatment~., data = trainData[,cname],family = binomial)
#prediction with testdata on logistic regression model
pred_reg <- predict(reg,newdata=testData[,cname],type='response')
#if result obtained is >0.5 prediction is of class "Yes", otherwise "No"
pred_reg <- ifelse(pred_reg > 0.5,"Yes","No")
t_reg<-table(predict=pred_reg,truth=testData$treatment)
#constructing a confusion matrix of the result
confusionMatrix(t_reg)
pred_reg_n <- convert_to_number(pred_reg)

#logistic regression with random forest
reg_rf = glm(trainData$treatment~., data = trainData[,signif_rf],family = binomial)
pred_reg_rf <- predict(reg_rf,newdata=testData[,signif_rf],type='response')
pred_reg_rf<- ifelse(pred_reg_rf > 0.5,"Yes","No")
t_reg_rf<-table(predict=pred_reg_rf,truth=testData$treatment)
confusionMatrix(t_reg_rf)
pred_reg_rf_n <- convert_to_number(pred_reg_rf)

#logistic regression with boruta
reg_boruta = glm(trainData$treatment~., data = trainData[,boruta_signif],family = binomial)
pred_reg_boruta <- predict(reg_boruta,newdata=testData[,boruta_signif],type='response')
pred_reg_boruta <- ifelse(pred_reg_boruta > 0.5,"Yes","No")
t_reg_boruta<-table(predict=pred_reg_boruta,truth=testData$treatment)
confusionMatrix(t_reg_boruta)
pred_reg_brt_n <- convert_to_number(pred_reg_boruta)

#logistic regression with chisq
reg_chisq = glm(trainData$treatment~., data = trainData[,subset_chisq],family = binomial)
pred_reg_chisq <- predict(reg_chisq,newdata=testData[,subset_chisq],type='response')
pred_reg_chisq <- ifelse(pred_reg_chisq > 0.5,"Yes","No")
t_reg_chisq<-table(predict=pred_reg_chisq,truth=testData$treatment)
confusionMatrix(t_reg_chisq)
pred_reg_chsq_n <- convert_to_number(pred_reg_chisq)

#logistic regression with ifg
reg_ifg = glm(trainData$treatment~., data = trainData[,subset_ifg],family = binomial)
pred_reg_ifg <- predict(reg_ifg,newdata=testData[,subset_ifg],type='response')
pred_reg_ifg <- ifelse(pred_reg_ifg > 0.5,"Yes","No")
t_reg_ifg<-table(predict=pred_reg_ifg,truth=testData$treatment)
confusionMatrix(t_reg_ifg)
pred_reg_ifg_n <- convert_to_number(pred_reg_ifg)

library(pROC)

treatment_n<-convert_to_number(testData[,'treatment'])
#constructing ROC plots
#plot1 is the ROC curve for all the classification models with all attributes
plot1<-plot(roc(as.numeric(pred_rf_n),as.numeric(treatment_n)), main="ROC with all features",print.auc = TRUE, col = "red")
plot1 <- plot(roc(as.numeric(pred_nb_n),as.numeric(treatment_n)), print.auc = TRUE, 
                             col = "green", print.auc.y = .4, add = TRUE)
plot1 <- plot(roc(as.numeric(pred_reg_n),as.numeric(treatment_n)), print.auc = TRUE, 
              col = "blue", print.auc.y = .8, add = TRUE)
legend("topleft",legend=c("Random Forest","Naive Bayes","Logistic Regression"),col=c("red","blue","green"),lty=c(1,1),cex = 0.75)
Sys.sleep(1)

#plot2 is the ROC curve for all the classification models with selected attributes from random forest feature selection technique
plot2<-plot(roc(as.numeric(pred_rf_rf_n),as.numeric(treatment_n)), main="ROC with features using Random Forest", print.auc = TRUE, col = "red")
plot2 <- plot(roc(as.numeric(pred_nb_rf_n),as.numeric(treatment_n)), print.auc = TRUE, 
              col = "green", print.auc.y = .4, add = TRUE)
plot2 <- plot(roc(as.numeric(pred_reg_rf_n),as.numeric(treatment_n)), print.auc = TRUE, 
              col = "blue", print.auc.y = .8, add = TRUE)
legend("topleft",legend=c("Random Forest","Naive Bayes","Logistic Regression"),col=c("red","blue","green"),lty=c(1,1),cex = 0.75)
Sys.sleep(1)

#plot3 is the ROC curve for all the classification models with selected attributes from boruta feature selection technique
plot3  <- plot(roc(as.numeric(pred_rf_brt_n),as.numeric(treatment_n)), main="ROC with features using Boruta",print.auc = TRUE, col = "red")
plot3 <- plot(roc(as.numeric(pred_nb_brt_n),as.numeric(treatment_n)), print.auc = TRUE, 
              col = "green", print.auc.y = .4, add = TRUE)
plot3  <- plot(roc(as.numeric(pred_reg_brt_n),as.numeric(treatment_n)), print.auc = TRUE, 
              col = "blue", print.auc.y = .8, add = TRUE)
legend("topleft",legend=c("Random Forest","Naive Bayes","Logistic Regression"),col=c("red","blue","green"),lty=c(1,1),cex = 0.75)
Sys.sleep(1)

#plot4 is the ROC curve for all the classification models with selected attributes from chi-square feature selection technique
plot4  <- plot(roc(as.numeric(pred_rf_chsq_n),as.numeric(treatment_n)), main="ROC with features using Chi- Square Test",print.auc = TRUE, col = "red")
plot4 <- plot(roc(as.numeric(pred_nb_chsq_n),as.numeric(treatment_n)), print.auc = TRUE, 
              col = "green", print.auc.y = .4, add = TRUE)
plot4  <- plot(roc(as.numeric(pred_reg_chsq_n),as.numeric(treatment_n)), print.auc = TRUE, 
               col = "blue", print.auc.y = .8, add = TRUE)
legend("topleft",legend=c("Random Forest","Naive Bayes","Logistic Regression"),col=c("red","blue","green"),lty=c(1,1),cex = 0.75)
Sys.sleep(1)

#plot2 is the ROC curve for all the classification models with selected attributes from mutual information selection technique
plot5  <- plot(roc(as.numeric(pred_rf_ifg_n),as.numeric(treatment_n)), main="ROC with features using Mutual Information", print.auc = TRUE, col = "red")
plot5 <- plot(roc(as.numeric(pred_nb_ifg_n),as.numeric(treatment_n)), print.auc = TRUE, 
              col = "green", print.auc.y = .4, add = TRUE)
plot5  <- plot(roc(as.numeric(pred_reg_ifg_n),as.numeric(treatment_n)), print.auc = TRUE, 
               col = "blue", print.auc.y = .8, add = TRUE)
legend("topleft",legend=c("Random Forest","Naive Bayes","Logistic Regression"),col=c("red","blue","green"),lty=c(1,1),cex = 0.75)

#svm classifer
cname<-c(cname,c("Age","treatment")) 

#convert "Yes" and "No" to 0 and 1
df_new[,"treatment"]<-convert_to_number(df_new[,"treatment"]) 

#make treatment as a categorical variable
df_new[,"treatment"]<-as.factor(df_new[,"treatment"])

df_new<-df_new[,!(names(df_new) %in% c("sl_no"))]

set.seed(2)
#split data into test and train
trainInp <- createDataPartition(y = df_new$treatment, p= 0.7, list = FALSE)
trainData_new <- df_new[trainInp,]
testData_new<- df_new[-trainInp,]

#set parameters for training the svm classifier
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

svm_Linear <- train(treatment ~., data = trainData_new, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"), #pre-process normalizes the data
                    tuneLength = 10)

#predict on test data and construct confusion matrix
pred_svm<- predict(svm_Linear, newdata = testData_new)
t_svm<-table(predict=pred_svm,truth=testData_new$treatment)
confusionMatrix(t_svm)
