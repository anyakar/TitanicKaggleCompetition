library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
library(party)
set.seed(415) 

# Load data
train <- read.csv(file="train.csv")
test <- read.csv(file="test.csv")
test$Survived <- NA
combi <- rbind(train, test)

#Engineering variables
combi$Name <- as.character(combi$Name)
#Title
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
combi$Title[combi$Title %in% c('Miss', 'Mlle')] <- 'Miss' #normalizing French titles - Madmoiselle
combi$Title[combi$Title %in% c('Mme', 'Mrs')] <- 'Mrs' #normalizing French titles - Madame
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir' #combining rich fellas
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady' #combining rich ladies
combi$Title <- factor(combi$Title)
table(combi$Title)
# FamilyID
combi$FamilySize <- combi$SibSp + combi$Parch + 1 # Family size 
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]}) #Family name
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="") #Paste together for family ID
combi$FamilyID[combi$FamilySize <= 2] <- 'Small' #Not concerned with small families
# more clean up
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,] #limit famID to small families (<=2 people)
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small' #family names get overwritten with small
combi$FamilyID <- factor(combi$FamilyID)

## Imputing missing values
summary(combi) #Age, Embarked, Fare
#Age
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),], method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])
#Embarked - 2 do not have values
which(combi$Embarked == '')
combi$Embarked[c(62,830)] = "S" #most common
#Fare - 1 does not have a value
which(is.na(combi$Fare))
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

# Split sets
train <- combi[1:891,]
test <- combi[892:1309,]

#### MODEL 1
#Random forest
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize +
                        FamilyID2, data=train, importance=TRUE, ntree=2000)
varImpPlot(fit) #importance of variables

Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "randomforest.csv", row.names = FALSE)


#### MODEL 2
# Forest of conditional inference trees - can handle large factors
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3))
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "conditionalforest_m2.csv", row.names = FALSE)


#### MODEL 3 - Adding "Single" as a category for FamilyID
# FamilyID - single person + three persons - to comp w/random forests
table(combi$FamilySize)
table(combi$FamilyID)
combi$FamilyID3 <- as.character(combi$FamilyID)
combi$FamilyID3[combi$FamilySize == 1] <- 'Single'
combi$FamilyID3[combi$FamilySize == 2] <- 'Two'
combi$FamilyID3[combi$FamilySize == 3] <- 'Three'
combi$FamilyID3 <- factor(combi$FamilyID3)
table(combi$FamilyID3)

## Compare to Random Forest
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize +
                        FamilyID3, data=train, importance=TRUE, ntree=2000)
varImpPlot(fit) #importance of variables
Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "randomforest_m3.csv", row.names = FALSE)


