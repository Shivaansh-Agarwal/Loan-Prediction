# MINI PROJECT-2
# Loan Approval Problem using Decision Tree

# PACKAGES USED
# library('ggplot2')
# library('rpart')
# library('rpart.plot')
# library(caret)
# library('C50')

#### PHASE-1 ####
## BUSINESS UNDERSTANDING
# See accompanying Document

#### PHASE-2 & PHASE-3 ####
## DATA UNDERSTANDING & DATA PREPARATION

# Setting the Working Directory
setwd("C:\Users\Shivaansh\Documents\GitHub\Loan-Prediction")

# TRAIN_SET
# Importing the Dataset
loan.train <- read.delim("Loans_Training", sep = ",", stringsAsFactor = TRUE, header=TRUE)
# Here we'll not use the Interest Column since it is 15% time the request amount times 3 years.
train_set <- loan.train[,-5]
# Encoding Categorical Variables
train_set$Approval <- factor(train_set$Approval, levels = c(TRUE,FALSE), labels = c(1,0))

head(train_set)
dim(train_set) # 150302 Rows and 4 Columns
names(train_set)
str(train_set)

# Count of NA values
sapply(train_set, function(x) sum(is.na(x))) # 0 NA values
# Count of empty strings
sapply(train_set, function(x) length(which(x==''))) # 0 Empty Strings
# Counting the Count of Number of Unique values in every Column
sapply(train_set, function(x) length(unique(x)))

# install.packages("ggplot2")
library(ggplot2)

# Debt.to.Income.Ratio
summary(train_set$Debt.to.Income.Ratio)
ggplot(data = train_set, aes(y=train_set$Debt.to.Income.Ratio)) + geom_boxplot() + ggtitle("Boxplot of Debt to Income Ratio")

# FICO.Score
summary(train_set$FICO.Score)
# Since maximum FICO Score can be 850 but as summary statistics show maximum fico score is of 869, 
# therefore it is an outlier and must be removed
ggplot(data = train_set, aes(y=train_set$FICO.Score)) + geom_boxplot() + ggtitle("Boxplot of FICO Score")
# Counting how many values are there above FICO Score of 850
length(which(train_set$FICO.Score>850)) # 8
# Now, let's remove these 8 rows
train_set <- train_set[-which(train_set$FICO.Score>850),]

# Request Amount
summary(train_set$Request.Amount)
ggplot(data = train_set, aes(y=train_set$Request.Amount)) + geom_boxplot() + ggtitle("Boxplot of Request Amount")

# Approval
table(train_set$Approval)

# Set up the plot area for Making Histograms of Debt. To Income Ratio, Fico Score, Request Amount
par(mfrow=c(1,3))
hist(train_set$Debt.to.Income.Ratio, col="blue", border="black", xlab="Ratio", ylab="Frequency", 
     main="Histogram of Debt to Income Ratio")
box(which="plot",lty="solid",col="black")
hist(train_set$FICO.Score, col="orange", border="black", xlab="FICO Score", ylab="Frequency", 
     main="Histogram of FICO Score")
box(which="plot",lty="solid",col="black")
hist(train_set$Request.Amount, col="green", border="black", xlab="Request Amount", ylab="Frequency", 
     main="Histogram of Request Amount")
box(which="plot",lty="solid",col="black")


# Now since the values in columns Debt.to.Income.Ratio, FICO.Score, Request.Amount vary greatly from one-another
# Let's scale these values using Z-Score Standardization
# train_set$DtIR.z <- (train_set$Debt.to.Income.Ratio - mean(train_set$Debt.to.Income.Ratio))/sd(train_set$Debt.to.Income.Ratio)
# train_set$FICOs.z <- (train_set$FICO.Score - mean(train_set$FICO.Score))/sd(train_set$FICO.Score)
# train_set$ReqAmt.z <- (train_set$Request.Amount - mean(train_set$Request.Amount))/sd(train_set$Request.Amount)
# train_set <- train_set[,-c(2,3,4)]

##########
# TEST_SET
# Importing the Dataset
loan.test <- read.delim("Loans_Test", sep = ",", stringsAsFactors = TRUE, header = TRUE)
test_set <- loan.test[,-5]
# Encoding Categorical Variables
test_set$Approval <- factor(test_set$Approval, levels = c(TRUE,FALSE), labels = c(1,0))

head(test_set)
dim(test_set) # 150302 Rows and 4 Columns
names(test_set)
str(test_set)

# Count of NA values
sapply(test_set, function(x) sum(is.na(x))) # 0 NA values
# Count of empty strings
sapply(test_set, function(x) length(which(x==''))) # 0 Empty Strings
# Counting the Count of Number of Unique values in every Column
sapply(test_set, function(x) length(unique(x)))

# Now since the values in columns Debt.to.Income.Ratio, FICO.Score, Request.Amount vary greatly from one-another
# Let's scale these values using Z-Score Standardization
# test_set$DtIR.z <- (test_set$Debt.to.Income.Ratio - mean(test_set$Debt.to.Income.Ratio))/sd(test_set$Debt.to.Income.Ratio)
# test_set$FICOs.z <- (test_set$FICO.Score - mean(test_set$FICO.Score))/sd(test_set$FICO.Score)
# test_set$ReqAmt.z <- (test_set$Request.Amount - mean(test_set$Request.Amount))/sd(test_set$Request.Amount)
# test_set <- test_set[,-c(2,3,4)]

#### PHASE-4 ####
## MODELING PHASE

## CART MODEL
# install.packages('rpart')
library('rpart')
model1 <- rpart(Approval ~ Debt.to.Income.Ratio + FICO.Score + Request.Amount, data = train_set, method = "class")
# Since y i.e. Approval (Dependent Variable) is a Categorical Variable therefore method = "class"
model1
summary(model1)

# Plotting the Decision Tree
# install.packages('rpart.plot')
library('rpart.plot')
rpart.plot(model1, main ="Classification Tree (CART MODEL)")

pred1 <- predict(object = model1, newdata = test_set, type = 'class')
table(test_set[,1],pred1)
library(caret)
confusionMatrix(data = pred1,reference = test_set$Approval)

## C5.0 MODEL
# install.packages('C50')
library('C50')
x <- train_set[,c(2,3,4)]
y <- train_set$Approval
model2 <-  C5.0(x,y)
model2
summary(model2)
# Pruning model2
model3 <- C5.0(x,y,control = C5.0Control(CF=0.1))
summary(model3)
plot(model3)

pred2=predict(model3,test_set,type="class")
table(test_set[,1],pred2)
confusionMatrix(data = pred2, reference = test_set$Approval)
