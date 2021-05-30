library(dplyr)
library(ggplot2)
library(caret)
library(e1071)
strokeData <- read.csv("stroke.csv")
str(strokeData)

#get a list of the column names
names(strokeData)

#Determine which predictors are statistically significant in determining whether a stroke will occur

#create data frame 
strokeDf <- data.frame(strokeData)
#view first 10 rows
head(strokeDf, 10)

#get a list of column names
names(strokeDf)

# Research Question: 

# Are there any chances of getting stroke to hypertension patients? we will start 
# by examining the impact of hypertension on stroke incidence for the patients involved.

#convert the stroke variable into a 2 level factor
strokeDf$stroke <- as.factor(strokeDf$stroke)
strokeDf$hypertension <- as.factor(strokeDf$hypertension)

strokeMod <- glm(stroke~hypertension, data = strokeDf, family='binomial')
summary(strokeMod)

b0 <- coef(strokeMod)
cat("The intercept coefficient value is", round(b0[1],3), 
    "which represents the odds of people without hypertension having a stroke.")


# make predictions with based on the data we have.

strokenohtn <- data.frame(hypertension="0")
nohtnpred <- predict(strokeMod, strokenohtn, type='response')
pctnohtn <- nohtnpred*100
cat("Patients without hypertension, within this group, have a ", 
    round(nohtnpred,5), " probability of stroke which indicates a ", 
    round(pctnohtn, 3), "% chance of having a stroke")

strokeWhtn <- data.frame(hypertension='1')
htnPred <- predict(strokeMod, strokeWhtn, type='response')
pctWhtn <- htnPred*100
cat("Hypertensive patients, within this group, have a", round(htnPred,5), 
    "probability of stroke which indicates a", round(pctWhtn, 3), "% chance of having a stroke.")


# Predictive Modelling

# we will build a predictive model to look at the impact of heart disease 
# on the risks of stroke in the patients included in this study.


#Model
hDmodStrokeMod <- glm(stroke~heart_disease,data = strokeDf, family='binomial')
summary(hDmodStrokeMod)


#Prediction/Validation
stroket <- strokeDf[1:2000,]
predicted_result <- predict(hDmodStrokeMod,newdata=stroket,type='response')
predicted_result2 <- ifelse(predicted_result > 0.5,1,0)
confusionMatrix(data=as.factor(predicted_result2), reference=as.factor(stroket$stroke))
predicted_result

#Forecasting
noHdpred <- data.frame(heart_disease=0)
noHD <- predict(hDmodStrokeMod, noHdpred, type='response')
pctnoHD <- noHD*100
cat("Patients without heart disease, within this group, have a ",
    round(noHD,3)," probability of stroke which indicates a ",
    round(pctnoHD,3)," % chance of having a stroke.")

hasHDdat <- data.frame(heart_disease=1)
yesHDpred <- predict(hDmodStrokeMod, hasHDdat, type='response')
pcthasHd <- yesHDpred*100
cat("Patients with heart disease, within this group, have a ",
    round(yesHDpred,3)," probability of stroke which indicates a ",
    round(pcthasHd,3)," % chance of having a stroke.")

