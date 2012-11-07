install.packages("randomForest")
require("randomForest")
require("plyr")

#Imports Training Data
train <- read.delim("~/Keggle_Project/train.tsv")
test <- read.delim("~/Keggle_Project/test.tsv")

#Counts Number of words in Training Data
counts <- sapply(gregexpr("\\W+", train[,3]), length) + 1
test_counts <- sapply(gregexpr("\\W+", test[,3]), length) + 1

maximums <- aggregate(train$grade,list(train$set),max)
maximums[4,2]=3
names(maximums)<-c("set","max")
#normalizes training data
train_norm <- ddply(train,.(set),transform,grade = grade / max(na.omit(grade)))

#Appends the normalized grade and the number of words to the set
train_norm <- cbind(train_norm,counts)
test <- cbind(test,test_counts)
test$grades=0
names(train_norm)
training_set <- train_norm[,c(2,6,7)]

testing_set <- as.data.frame(test[,c(2,4,5)])
names(testing_set)<-c("set","counts","grade")

#Regression with randomForest
set.seed(131)
grades.rf <- randomForest(grade ~., data = training_set,
                              importance=TRUE, na.action=na.omit)
estimates <-(predict(grades.rf,testing_set))

train_estimates <-(predict(grades.rf,training_set))
#de-normalize estimates
estimates <-  as.data.frame(cbind(estimates,test[,2]))
estimates$denorm <- 0
names(estimates) <- c("est","set")
for (i in seq(nrow(estimates))){
    mult <- which(maximums$set == estimates[i,2])
    estimates[i,3] <- round(estimates[i,1]*maximums[mult,2])
}
write.table(estimates,file="randomForestTest.txt")



