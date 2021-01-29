install.packages("randomForest")
library(randomForest)
install.packages("MASS")
library(MASS)
install.packages("caret")
library(caret)

set.seed(212)
FraudCheck <- read.csv(file.choose())
View(FraudCheck)
hist(FraudCheck$Taxable.Income)

hist(FraudCheck$Taxable.Income, main = "sales of CompanyData",xlim = c(0,1000),
     breaks = c(seq(40,60,80)), col = c("blue","red","green","yellow"))

Risky_Good = ifelse(FraudCheck$Taxable.Income<=30000, "Risky", "Good")
FCtemp = data.frame(FraudCheck,Risky_Good)
FC = FCtemp[,c(1:7)]
str(FC)
FC[sapply(FC,is.character)] <- lapply(FC[sapply(FC,is.character)],as.factor)
str(FC)

table(FC$Risky_Good)
set.seed(223)
ind <- sample(2,nrow(FC),replace = TRUE, prob = c(0.7,0.3))
train <- FC[ind==1,]
test <- FC[ind==2,]
rf <- randomForest(Risky_Good~., data = train)
rf

attributes(rf)

pred1 <- predict(rf,train)
pred1
head(pred1)


confusionMatrix(pred1, train$Risky_Good)

pred2 <- predict(rf,test)
pred2
head(pred2)

confusionMatrix(pred2, test$Risky_Good)

plot(rf)
#there is a constant line after 100 trees

tune <- tuneRF(train[,-6], train[,6], stepFactor = 0.5, plot = TRUE, ntreeTry = 300,
               trace = TRUE,improve = 0.05)
rf1 <- randomForest(Risky_Good~., data=train, ntree=200, mtry=2, importance=TRUE,
                    proximity=TRUE)
rf1

pred1 <- predict(rf1,train)
pred1
head(pred1)


confusionMatrix(pred1, train$Risky_Good)

pred2 <- predict(rf1,test)
pred2
head(pred2)

confusionMatrix(pred2, test$Risky_Good)

hist(treesize(rf1),main = "NO of nodes for trees", col = "green")

varImpPlot(rf1)

importance(rf1)

partialPlot(rf1,train, Taxable.Income,"Good")

tr1 <- getTree(rf1, 2, labelVar = TRUE)

# Multi Dimension scaling plot of proximity Matrix
MDSplot(rf1, FC$Risky_Good)

####################companydata##############

set.seed(101)
companyData <- read.csv(file.choose())
View(companyData)
hist(companyData$Sales)

hist(companyData$Sales, main = "sales of CompanyData",xlim = c(0,20),
     breaks = c(seq(10,20,30)), col = c("blue","red","green","yellow"))

str(companyData)

highsales = ifelse(companyData$Sales<9, "No", "Yes")
CD = data.frame(companyData[2:11],highsales)


CD[sapply(CD,is.character)] <- lapply(CD[sapply(CD,is.character)],as.factor)
str(CD)

table(CD$highsales)
set.seed(101)
ind <- sample(2,nrow(CD),replace = TRUE, prob = c(0.7,0.3))
train <- CD[ind==1,]
test <- CD[ind==2,]
rf <- randomForest(highsales~., data = train)
rf

attributes(rf)

pred1 <- predict(rf,train)
pred1
head(pred1)


confusionMatrix(pred1, train$highsales)

pred2 <- predict(rf,test)
pred2
head(pred2)

confusionMatrix(pred2, test$highsales)

plot(rf)

tune <- tuneRF(train[,-11], train[,11], stepFactor = 0.5, plot = TRUE, ntreeTry = 300,
               trace = TRUE,improve = 0.05)
rf1 <- randomForest(highsales~., data=train, ntree=200, mtry=2, importance=TRUE,
                    proximity=TRUE)
rf1

pred1 <- predict(rf1,train)
pred1
head(pred1)


confusionMatrix(pred1, train$highsales)


pred2 <- predict(rf1,test)
pred2
head(pred2)

confusionMatrix(pred2, test$highsales)

hist(treesize(rf1),main = "NO of nodes for trees", col = "green")

varImpPlot(rf1)

importance(rf1)


