#-----extract feature---
library(dplyr)
library(plyr) #ddply

#----set file path----
setwd('/Users/fuyincherng/Documents/EPFLCourse/DigitalEducation/R/cs411Project/CS411Project')

#------ read data frame
db=read.csv('FirstFeatures/OutputTable_train.csv')
#db=read.csv('FirstFeatures/OutputTable_test.csv')
#------ sort submissions
db=db[order(db$UserID,db$ProblemID,db$SubmissionNumber),]
dim(db) #dimention of the data
View(db) #show the table of data
#------- aggregate by UserID and ProblemID ---------
  length(unique(db$UserID))
  agg.features=ddply(db, .(UserID,ProblemID), summarise, 
          overalGradeDiff=Grade[length(Grade)]-Grade[1], 
          avgTbwSubs = sum(TimeSinceLast, na.rm = T)/ length(SubmissionNumber),
          CountofSubmissions=length(SubmissionNumber),
          countOfVideoandForumEvents= (sum(NVideoEvents,na.rm = T) + sum(NForumEvents,na.rm = T)),
          countOfVideoEvents= sum(NVideoEvents,na.rm = T),
          countOfForumEvents= sum(NForumEvents,na.rm = T),
          CountofDurationVideoActivity = sum(DurationOfVideoActivity,na.rm=T),
          CountofThreadView = sum(NumberOfThreadViews,na.rm = T)
          )
#------ remove cases with only one attempt
  agg.features=filter(agg.features,CountofSubmissions>1); 
  dim(agg.features)
  View(agg.features)
#------ cal the correlation between overalGradDiff and features
  cor(agg.features$overalGradeDiff, agg.features$avgTbwSubs)
  cor(agg.features$overalGradeDiff, agg.features$countOfVideoandForumEvents)
  cor(agg.features$overalGradeDiff, agg.features$countOfVideoEvents)
  cor(agg.features$overalGradeDiff, agg.features$countOfForumEvents)
  cor(agg.features$overalGradeDiff, agg.features$CountofDurationVideoActivity)
  cor(agg.features$overalGradeDiff, agg.features$CountofThreadView)
#------ save feature file
  write.csv(agg.features, file='SecondFeatures/secondfeatures_train.csv')
  #write.csv(agg.features, file='SecondFeatures/secondfeatures_test.csv')
  
 