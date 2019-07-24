#Reading in the draft kings data

library(readr)

DK1 <- read_csv("C:/Users/mille/OneDrive/Documents/ST-558-Final-Project-Draft-Kings/CSV files/DK11-27.csv")

DK2 <- read_csv("C:/Users/mille/OneDrive/Documents/ST-558-Final-Project-Draft-Kings/CSV files/DK11-28.csv")

DK3 <- read_csv("C:/Users/mille/OneDrive/Documents/ST-558-Final-Project-Draft-Kings/CSV files/DK11-29.csv")

DK4 <- read_csv("C:/Users/mille/OneDrive/Documents/ST-558-Final-Project-Draft-Kings/CSV files/DK11-30.csv")

DK5 <- read_csv("C:/Users/mille/OneDrive/Documents/ST-558-Final-Project-Draft-Kings/CSV files/DK12-1.csv")

DK6 <- read_csv("C:/Users/mille/OneDrive/Documents/ST-558-Final-Project-Draft-Kings/CSV files/DK12-2.csv")

DK7 <- read_csv("C:/Users/mille/OneDrive/Documents/ST-558-Final-Project-Draft-Kings/CSV files/DK12-3.csv")

DK8 <- read_csv("C:/Users/mille/OneDrive/Documents/ST-558-Final-Project-Draft-Kings/CSV files/DK12-4.csv")

DK9 <- read_csv("C:/Users/mille/OneDrive/Documents/ST-558-Final-Project-Draft-Kings/CSV files/DK12-5.csv")

DK10 <- read_csv("C:/Users/mille/OneDrive/Documents/ST-558-Final-Project-Draft-Kings/CSV files/DK12-6.csv")

DK11 <- read_csv("C:/Users/mille/OneDrive/Documents/ST-558-Final-Project-Draft-Kings/CSV files/DK12-7.csv")

DK12 <- read_csv("C:/Users/mille/OneDrive/Documents/ST-558-Final-Project-Draft-Kings/CSV files/DK12-8.csv")

DK13 <- read_csv("C:/Users/mille/OneDrive/Documents/ST-558-Final-Project-Draft-Kings/CSV files/DK12-9.csv")

DK14 <- read_csv("C:/Users/mille/OneDrive/Documents/ST-558-Final-Project-Draft-Kings/CSV files/DK12-10.csv")

DK15 <- read_csv("C:/Users/mille/OneDrive/Documents/ST-558-Final-Project-Draft-Kings/CSV files/DK12-11.csv")

DK16 <- read_csv("C:/Users/mille/OneDrive/Documents/ST-558-Final-Project-Draft-Kings/CSV files/DK12-12.csv")

DK17 <- read_csv("C:/Users/mille/OneDrive/Documents/ST-558-Final-Project-Draft-Kings/CSV files/DK12-13.csv")

DK18 <- read_csv("C:/Users/mille/OneDrive/Documents/ST-558-Final-Project-Draft-Kings/CSV files/DK12-14.csv")

DK19 <- read_csv("C:/Users/mille/OneDrive/Documents/ST-558-Final-Project-Draft-Kings/CSV files/DK12-15.csv")

DK20 <- read_csv("C:/Users/mille/OneDrive/Documents/ST-558-Final-Project-Draft-Kings/CSV files/DK12-16.csv")

DK21 <- read_csv("C:/Users/mille/OneDrive/Documents/ST-558-Final-Project-Draft-Kings/CSV files/DK12-18.csv")

DK22 <- read_csv("C:/Users/mille/OneDrive/Documents/ST-558-Final-Project-Draft-Kings/CSV files/DK12-19.csv")

DK23 <- read_csv("C:/Users/mille/OneDrive/Documents/ST-558-Final-Project-Draft-Kings/CSV files/DK12-20.csv")

DK24 <- read_csv("C:/Users/mille/OneDrive/Documents/ST-558-Final-Project-Draft-Kings/CSV files/DK12-21.csv")

DK25 <- read_csv("C:/Users/mille/OneDrive/Documents/ST-558-Final-Project-Draft-Kings/CSV files/DK12-22.csv")

DK26 <- read_csv("C:/Users/mille/OneDrive/Documents/ST-558-Final-Project-Draft-Kings/CSV files/DK12-23.csv")

DK27 <- read_csv("C:/Users/mille/OneDrive/Documents/ST-558-Final-Project-Draft-Kings/CSV files/DK12-25.csv")

DF <- rbind(DK1,DK2,DK3,DK4,DK5,DK6,DK7,DK8,DK9,DK10,DK11,DK12,DK13,DK14,DK15,DK16,DK17,DK18,DK19,DK20,DK21,DK22,DK23,DK24,DK25,DK26, DK27)

library(dplyr)

draftKingsData <- DF %>% filter(DF$Inj!="O" | is.na(DF$Inj)==TRUE)

draftKingsData$`Player Name` <- as.factor(draftKingsData$`Player Name`)
length(levels(draftKingsData$`Player Name`))

draftKingsData$player <- as.character(draftKingsData$`Player Name`)
draftKingsData$player <- as.factor(draftKingsData$player)

draftKingsData$`Inj` <- ifelse(is.na(draftKingsData$Inj), "None", draftKingsData$Inj)

draftKingsData$Inj <- as.factor(draftKingsData$Inj)
length(levels(draftKingsData$Inj))


draftKingsData$Pos <- as.factor(draftKingsData$Pos)

draftKingsData$Team <- as.factor(draftKingsData$Team)

draftKingsData$Opp <- as.factor(draftKingsData$Opp)

draftKingsData$Rest <- as.numeric(draftKingsData$Rest)

draftKingsData$`Opp DvP` <- substr(draftKingsData$`Opp DvP`, 1, nchar(draftKingsData$`Opp DvP`, type="chars")-1)

draftKingsData$`Opp DvP` <- as.numeric(draftKingsData$`Opp DvP`)

str(draftKingsData)

# draftKingsData$perOverProj <- round((draftKingsData$`Actual FP` - draftKingsData$`Proj FP`)/draftKingsData$`Proj FP`,3)


#Dataset of variables to use for predictions
predData <- draftKingsData %>% select(-Likes, -PS, -`Proj FP`, -`Proj Val`, -`Actual Val`, -`Actual Min`)


#Modeling

#Create training set and testing set
set.seed(31)
train <- sample(1:nrow(predData), size = nrow(predData)*0.8)
test <- dplyr::setdiff(1:nrow(predData), train)
predDataTrain <- predData[train, ]
predDataTest <- predData[test, ]

#Fitting the Model

fullFit <-lm(`Actual FP` ~ ., data = predDataTrain)
step(fullFit)

lmFit <- lm(formula = `Actual FP` ~ `Player Name` + Inj + Salary + Rest + 
              USG + PER + `Opp Pace` + `Opp DEff` + `L5 FGA` + `S Min` + 
              `S FP` + `Proj Min`, data = predDataTrain)

lmPred <- predict(lmFit, newdata = predDataTrain)

