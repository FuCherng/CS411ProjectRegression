#----import lib
library(dplyr)
library(plyr) #ddply
library(caret)
library(lattice)
library(ggplot2)


#----import the second features csv
setwd('/Users/fuyincherng/Documents/EPFLCourse/DigitalEducation/R/cs411Project/CS411Project')
db_r<-read.csv('SecondFeatures/secondfeatures_train.csv', stringsAsFactors = F)

#----train the regression model
db_r<-db_r[order(db_r$UserID,db_r$ProblemID,db_r$SubmissionNumber),]
dim(db_r) #dimention of the data
#View(db_r) #show the table of data

#--- replace NA values with 0
db_r[is.na(db_r)]=0

#---- plot features
#hist
hist(db_r$overalGradeDiff)

#scatterplot 
plot(db_r$overalGradeDiff, db_r$avgTbwSubs, main="Scatterplot Example", 
     xlab="overalGradeDiff ", ylab="avgTbwSubs ", pch=19) 
abline(lm(db_r$avgTbwSubs~db_r$overalGradeDiff), col="red") # regression line (y~x) 

plot(db_r$overalGradeDiff, db_r$CountofSubmissions, main="Scatterplot Example", 
     xlab="overalGradeDiff ", ylab="avgTbwSubs ", pch=19) 
abline(lm(db_r$CountofSubmissions~db_r$overalGradeDiff), col="red") # regression line (y~x) 


# ----- split your training data into train and test set. 
set.seed(1234)
tr.index= sample(nrow(db_r), nrow(db_r)*0.8)
db_r.train= db_r[tr.index,]
db_r.test = db_r[-tr.index,]
dim(db_r.train)
dim(db_r.test)
hist(db_r.train$overalGradeDiff)
hist(db_r.test$overalGradeDiff)

#----testing the regression model
fs <- c('avgTbwSubs',
        'CountofSubmissions'
        )


#----- train model
#lm
paramGrid <- expand.grid(intercept = c(0.1,1,10))
ctrl <- trainControl(method = 'cv',classProbs = TRUE)
model <- train(x=db_r.train[,fs],
               y=db_r.train$overalGradeDiff,
               method = "lm",
               #metric="ROC",
               #trControl = ctrl,
               tuneGrid = paramGrid,
               preProc = c("center", "scale")
)
print(model);   plot(model)  #RMSE will be reported in print(model)

#rf
#svmLinear2 or others


#---- feature selection
#estimate variable importance
importance <- varImp(model, scale=FALSE)
print(importance);  plot(importance)

#----- check generalizability of your model on new data
preds= predict(model, newdata=db_r.test[,fs]);
summary(preds)
library(Metrics)
rmse(db_r.test$overalGradeDiff, preds) #rmse(sim, obs, na.rm=TRUE, ...)

#============ test ============
#----import the second features of test data set
testDb<-read.csv('SecondFeatures/secondfeatures_test.csv', stringsAsFactors = F)
testDb$overalGradeDiff <- NULL
testDb[is.na(testDb)]=0

#----apply Regression model to test data
preds_test= predict(model, newdata=testDb[,fs]);
summary(preds_test)

#======================================================================== 
#         step 2.1: prepare submission file for kaggle
#======================================================================== 
#----prepare submission to Kaggle
Reg.Results=testDb[,c('ProblemID', 'UserID')]
Reg.Results$overalGradeDiff=preds_test
#levels(cl.Results$improved)=c(0,1) # 
Reg.Results$uniqRowID= paste0(Reg.Results$UserID,'_', Reg.Results$ProblemID)
Reg.Results=Reg.Results[,c('uniqRowID','overalGradeDiff')]
summary(Reg.Results$overalGradeDiff)

#----- keep only rows which are listed in regression_templtae.csv file
setwd('/Users/fuyincherng/Documents/EPFLCourse/DigitalEducation/R/cs411Project/CS411Project')
classifier_templtaete= read.csv('regression_template.csv', stringsAsFactors = F)
kaggleSubmission<-merge(classifier_templtaete,Reg.Results, xlab='uniqRowID', ylab='uniqRowID')
write.csv(kaggleSubmission,file='RegressionResult/regression_results_lm_1901.csv', row.names = F)



