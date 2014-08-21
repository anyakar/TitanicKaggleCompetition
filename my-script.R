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
combi$FamilyID1 <- combi$FamilyID
combi$FamilyID1[combi$FamilySize <= 2] <- 'Small' #Not concerned with small families
#Adding FamilyID 'Small' - model #1
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,] #limit famID to small families (<=2 people) - this is Freq - 
combi$FamilyID1[combi$FamilyID1 %in% famIDs$Var1] <- 'Small' #family names get overwritten with small
combi$FamilyID1 <- factor(combi$FamilyID1)
str(combi)
# AK - families where Freq & FamilyID don't match:
FamIDs <- data.frame(table(combi$FamilyID))
FamIndex <- (sub("[A-Z][a-z]+", "\\1", famIDs$Var1) != as.character(famIDs$Freq))
FamilyOdd <- famIDs[FamIndex,] #193 out of 928 FamilyIDs are ODD
# Cleaning up
OddRows <- combi[which(combi$FamilyID %in% FamilyOdd$Var1[1]),]
for (i in 2:nrow(FamilyOdd)) {
    OddRows <- rbind(OddRows, combi[which(combi$FamilyID %in% FamilyOdd$Var1[i]),])
    }
#SEE manual clean up doc
combi$FamilyIDCleanUp <- as.character(combi$FamilyID)
combi$FamilyIDCleanUp2 <- as.character(combi$FamilyIDCleanUp2)
combi$FamilySizeCleanUp <- combi$FamilySize
#run the cleaning stuff
combi$FamilyIDCleanUp <- as.factor(combi$FamilyIDCleanUp)
combi$FamilySizeCleanUp <- as.integer(combi$FamilySizeCleanUp)
combi$FamilyIDCleanUp2 <- combi$FamilyIDCleanUp
combi$FamilyIDCleanUp2[combi$FamilySizeCleanUp <= 2] <- 'Small'
combi$FamilySizeCleanUp <- as.integer(combi$FamilySizeCleanUp)
combi$FamilyIDCleanUp2 <- as.factor(combi$FamilyIDCleanUp2)

combi$FamilyID <- factor(combi$FamilyID)
#familytable <- aggregate(FamilyID ~ FamilyID + Name + FamilySize + PassengerId, data=combi, FUN=length) # how many are in a given Family ID
#familytable <- aggregate(FamilyID ~ FamilySize, data=combi, FUN=length) # how many are in a given Family ID
#Preping FamilyID for Random Forests (<32 factor levels)
combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)
# FamilyID - single person + three persons - to comp w/random forests
table(combi$FamilySize)
table(combi$FamilyID)
combi$FamilyID3 <- as.character(combi$FamilyID)
combi$FamilyID3[combi$FamilySize == 1] <- 'Single'
combi$FamilyID3[combi$FamilySize == 2] <- 'Two'
combi$FamilyID3[combi$FamilySize == 3] <- 'Three'
combi$FamilyID3 <- factor(combi$FamilyID3)
table(combi$FamilyID3, combi$FamilySize)
table(combi$FamilyID3, combi$FamilySize, combi$Parch)
aggregate(FamilyID3 ~ SibSp + Parch, data=combi, FUN=function(x) {round(sum(x)/length(x), 3)})


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
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID1,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3))
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "conditionalforest_m2b.csv", row.names = FALSE)


#### MODEL 3 - Adding "Single" as a category for FamilyID
## Compare to Random Forest
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize +
                        FamilyID3, data=train, importance=TRUE, ntree=2000)
varImpPlot(fit) #importance of variables
Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "randomforest_m3.csv", row.names = FALSE)

#### MODEL 4 - Using new FamilyID3 with Conditional Forests
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID3,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3))
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "conditionalforest_m4.csv", row.names = FALSE)


#### MODEL 5 - 
# Families with infants?
# Male parent of the only child?
combi$SingleParent <- as.character(combi$SingleParent)
combi$SingleParent <- 'NotRemarkable'
#combi$SingleParent[(combi$Parch == 1) & (combi$Title %in% c('Mrs', 'Mr','Col', 'Dr', 'Lady', 'Rev','Sir'))] <- 'SingleParent'
combi$SingleParent[(combi$Parch == 1) & (combi$Title %in% c('Mr','Col', 'Dr', 'Rev','Sir'))] <- 'SingleFather'
combi$SingleParent[(combi$Parch == 1) & (combi$Title %in% c('Miss', 'Master'))] <- 'OnlyChild'
combi$SingleParent[(combi$Parch == 1) & (combi$Title %in% c('Miss', 'Master')) & (combi$Age <= 1)] <- 'Infant'
combi$SingleParent <- factor(combi$SingleParent)

combi$ChildParent <- as.character(combi$ChildParent)
combi$ChildParent <- 'NotRemarkable'
combi$ChildParent[(combi$Parch == 1) & (combi$SibSp == 0) & (combi$Title %in% c('Mr','Col', 'Dr', 'Rev','Sir'))] <- 'SingleParentMale'
#combi$ChildParent[(combi$Parch == 1) & (combi$Title %in% c('Mrs', 'Lady'))] <- 'SingleParentFemale'
#combi$ChildParent[(combi$Parch == 1) & (combi$Title %in% c('Miss', 'Master')) & (combi$Age <= 16)] <- 'OnlyChildUnder16'
#combi$ChildParent[(combi$Parch == 1) & (combi$Title %in% c('Miss', 'Master')) & (combi$Age <= 12)] <- 'OnlyChildUnder12'
combi$ChildParent[(combi$Parch == 1) & (combi$Title %in% c('Miss', 'Master')) & (combi$Age < 6)] <- 'OnlyChildUnder6'
#combi$ChildParent[(combi$Parch == 1) & (combi$Title %in% c('Miss', 'Master')) & (combi$Age <= 1)] <- 'Infant'
combi$ChildParent[(combi$Title %in% c('Miss', 'Master')) & (combi$Age <= 1)] <- 'Infant'
combi$ChildParent <- as.factor(combi$ChildParent)

#more options - parent of infant
infants <- subset(combi, combi$Age <= 1) #infants
infants <- subset(combi, ((combi$Age <= 1) & (combi$Parch == 1))) #infants with single parent
parentIDs <- as.character(infants$FamilyID) # infants with single parent
smallchildren <- subset(combi, ((combi$Age > 1) & (combi$Age <= 3) & (combi$Parch == 1))) #with single parent
parentIDs <- as.character(c(infants$FamilyID, smallchildren$FamilyID))
parentIDsFreq <- data.frame(table(parentIDs))
parentIDs1 <- parentIDs[parentIDsFreq$Freq ==1]
parentIDs2 <- parentIDs[parentIDsFreq$Freq ==2]
combi$ChildParent <- as.character(combi$ChildParent)
combi$ChildParent[(combi$FamilyID %in% parentIDs1) & (combi$Title %in% c('Mrs', 'Mr','Col', 'Dr', 'Lady', 'Rev','Sir'))] <- 'SingleParentOfSmChild'
combi$ChildParent[(combi$FamilyID %in% parentIDs2) & (combi$Title %in% c('Mrs', 'Mr','Col', 'Dr', 'Lady', 'Rev','Sir'))] <- 'TwoParentsOfSmChild'
#combi$ChildParent[(combi$FamilyID %in% parentIDs) & (combi$Title %in% c('Mrs', 'Mr','Col', 'Dr', 'Lady', 'Rev','Sir'))] <- 'ParentOfInfant'
combi$ChildParent[(combi$Title %in% c('Miss', 'Master')) & (combi$Age <= 1)] <- 'Infant'
#combi$ChildParent[(combi$Parch == 1) & (combi$Title %in% c('Mrs', 'Lady'))] <- 'SingleParentFemale'

combi$ChildParent <- factor(combi$ChildParent)
combi$FamilyIDCleanUp <- factor(combi$FamilyIDCleanUp)

fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID3 + singleParent,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3))
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "conditionalforest_m5.csv", row.names = FALSE)

fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID1 + singleParent,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3))
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "conditionalforest_m6.csv", row.names = FALSE)

fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID3 + ChildParent,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3))
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "conditionalforest_m7.csv", row.names = FALSE)

fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID3 + ChildParent,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3))
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "conditionalforest_m8.csv", row.names = FALSE)

fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID3 + SingleParent,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3))
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "conditionalforest_m9.csv", row.names = FALSE)

fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID3 + ChildParent,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3))
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "conditionalforest_m10.csv", row.names = FALSE)

fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID1 + ChildParent,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3))
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "conditionalforest_m11.csv", row.names = FALSE)

fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID1 + ChildParent,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3))
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "conditionalforest_m12.csv", row.names = FALSE)

fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID1 + ChildParent,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3))
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "conditionalforest_m13.csv", row.names = FALSE)

fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySizeCleanUp + FamilyIDCleanUp2,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3))
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "conditionalforest_m14.csv", row.names = FALSE)

fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySizeCleanUp + FamilyIDCleanUp2 + ChildParent,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3))
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "conditionalforest_m15.csv", row.names = FALSE)

fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + Fare + Title + 
                   FamilySizeCleanUp + FamilyIDCleanUp2 + ChildParent,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3))
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "conditionalforest_m16.csv", row.names = FALSE)

####
write.csv(combi, file = "combi.csv", row.names = FALSE)