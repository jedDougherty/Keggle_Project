install.packages("randomForest")
require("randomForest")
require("plyr")

#Imports Training Data
train <- read.delim("~/Keggle_Project/train.tsv")
test <- read.delim("~/Keggle_Project/test.tsv")

#Counts Number of words in Training Data and adds column to training df
counts <- sapply(gregexpr("\\W+", train[,3]), length) + 1
train <- cbind(train,counts)

#counts number of words in testing data and adds column to testing df
test_counts <- sapply(gregexpr("\\W+", test[,3]), length) + 1
test <- cbind(test,test_counts)
test$grades=0

maximums <- aggregate(train$grade,list(train$set),max)
minimums <- aggregate(train$grade,list(train$set),min)
maximums[4,2]=3
minimums[4,2]=0
names(maximums)<-c("set","max")
#normalizes training data
train_norm <- ddply(train,.(set),transform,grade = (grade-min(na.omit(grade))) / (max(na.omit(grade))-min(na.omit(grade))))
train_norm <- ddply(train_norm,.(set),transform,counts = (counts-min(na.omit(counts))) / (max(na.omit(counts))-min(na.omit(counts))))

#normalizes testing wordcounts
test <- ddply(test,.(set),transform,test_counts = (test_counts-min(na.omit(test_counts))) / (max(na.omit(test_counts))-min(na.omit(test_counts))))

#Appends the normalized grade and the number of words to the set

training_set <- train_norm[,c(2,6,7)]
testing_set <- test[,c(2,4,5)]
names(testing_set)<-c("set","counts","grade")

#Regression with randomForest
set.seed(131)
grades.rf <- randomForest(grade ~ ., data = training_set,
                              importance=TRUE, na.action=na.omit, ntree = 1000)
estimates <-(predict(grades.rf,testing_set))

train_estimates <-(predict(grades.rf,training_set))


plot(training_set$grade,train_estimates)
plot(estimates)
#de-normalize estimates


estim <-  as.data.frame(cbind(estimates,testing_set[,1]))
estim$denorm <- 0
names(estim) <- c("est","set","denorm")
    for (i in seq(nrow(estim))){
        mult <- which(maximums$set == estim[i,2])
        estim[i,3] <- round(estim[i,1]*(maximums[mult,2]-minimums[mult,2])+minimums[mult,2])
    }

write.table(estim,file="randomForestTest.txt")



##################Data Investigation

#function given a data.frame with columns set,grade,counts
#returns the plot of grade v counts for that set.
#Put data.frame into group. put a number into the set_num 
#variable
sets <- function(group,set_num){
  a <- group[group$set==set_num,]
  b <- plot(a$grade,a$counts)
  return(b)
}



