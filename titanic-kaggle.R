# Reading the train set of data:
train <- read.csv(file="train.csv")
str(train)
table(train$Survived)
prop.table(table(train$Survived))

# Women and children first
summary(train$Sex)
table(train$Sex)

prop.table(table(train$Sex, train$Survived))
prop.table(table(train$Sex, train$Survived),1) #proportions by first dimension

# Nobody survived (0.62) 
test <- read.csv(file="test.csv")
test$Survived <- rep(0, 418)  #populate all with 0 

# Gender only model (0.7655) -- female passangers survived and male didn't 
test$Survived <- 0 #same as above
test$Survived[test$Sex == 'female'] <- 1

# Age and gender model
summary(train$Age) #177 NAs
train$Child <- 0
train$Child[train$Age < 18] <- 1 #NAs are 0, they fail boolean test

aggregate(Survived ~ Child + Sex, data=train, FUN=sum) # how many survived
aggregate(Survived ~ Child + Sex, data=train, FUN=length) #how many total
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {round(sum(x)/length(x), 3)})
aggregate(Survived ~ Age + Sex, data=train, FUN=function(x) {round(sum(x)/length(x), 3)})

train$Child <- 0
train$Child[train$Age < 13] <- 1 #child is 12 and younger
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {round(sum(x)/length(x), 3)})

# Class/Fare model
summary(train$Fare == 0)

#Fare == 0
aggregate(Survived ~ Child + Sex + (train$Fare == 0), data=train, 
          FUN=function(x) {round(sum(x)/length(x), 3)})
#15 males, survival rate 0.067, none are children
aggregate(Survived ~ Sex + (train$Fare == 0), data=train, 
          FUN=sum)
#Tets set - there are 2 males who paid Fare of 0

#More on fare
library(ggplot2)
plot(x=train$Fare, y=train$Survived)
trainmale <- subset(train, train$Sex == 'male')
ggplot(data = trainmale, aes(x = Fare, y = Age, color = Survived)) +
    geom_point()
testmale <- subset(test, test$Sex == 'male')
ggplot(data = test, aes(x = Fare, y = Age, color = Sex)) +
    geom_point()

train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'

aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})
#learn that females in 3 class who paid >20 had a worse chance of survival

# Decision trees 
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

#(0.785)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")
plot(fit)
text(fit)
fancyRpartPlot(fit)

#(0.74163) override defaults - overfitted model
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train,
             method="class", control=rpart.control(minsplit=2, cp=0))
fancyRpartPlot(fit)
#but it could be snipped
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train,
             method="class", control=rpart.control( your controls ))
new.fit <- prp(fit,snip=TRUE)$obj
fancyRpartPlot(new.fit)

# Feature Engineering
# Combine datasets first
test$Survived <- NA
combi <- rbind(train, test)

# Names -> FamilyID and Title
combi$Name <- as.character(combi$Name)
strsplit(combi$Name[1], split='[,.]')
# Names -> Title
strsplit(combi$Name[1], split='[,.]')[[1]][2] #to get title
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title) #strip extra spaces
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle' #combining French names
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir' #combining rich fellas
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady' #combining rich ladies
combi$Title <- factor(combi$Title)

# FamilyID
combi$FamilySize <- combi$SibSp + combi$Parch + 1 # Family size 
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]}) #Family name
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="") #Paste together for family ID
combi$FamilyID[combi$FamilySize <= 2] <- 'Small' #Not concerned with small families
# more clean up
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small' #family names get overwritten with small
combi$FamilyID <- factor(combi$FamilyID)

# Split sets
train <- combi[1:891,]
test <- combi[892:1309,]

# Model w/Engineered Features
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
             data=train, method="class")

## Dealing with NAs
summary(combi) #Age, Embarked, 
#Age
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),], method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])
#Embarked
summary(combi$Embarked) #2 do not have values
which(combi$Embarked == '')
combi$Embarked[c(62,830)] = "S" #most common
#Fare
summary(combi$Fare)
which(is.na(combi$Fare))
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

#Preping FamilyID for Random Forests (<32 factor levels)
combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)
#Ready
install.packages('randomForest')
library(randomForest)
set.seed(415) #to get a consistent forest
#Random forest
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize +
                        FamilyID2, data=train, importance=TRUE, ntree=2000)
varImpPlot(fit) #importance of variables

# Forest of conditional inference trees - can handle large factors
install.packages('party')
library(party)
set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3))
Prediction <- predict(fit, test, OOB=TRUE, type = "response")


#_________________________
# Some rules
train$Child[train$Age < 13] <- 1 #children 12 and younger had a better chance of survival
test$Survived[test$Fare == 0] <- 0  #Those who paid a fare of 0, did not survive (males)

# Submit data to Kaggle:
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)

# Submit from decision tree:
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)

# Submit from random forest:
Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "firstforest.csv", row.names = FALSE)


