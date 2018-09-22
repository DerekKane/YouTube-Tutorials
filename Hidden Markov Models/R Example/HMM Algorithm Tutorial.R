#################################################################################
# Hidden Markov Model Example
#################################################################################

library(quantmod)
getSymbols("^TWII", src = "yahoo", from = "1900-01-01", to = "2015-12-31")

chartSeries(TWII, theme="black")


TWII_Subset <- window(TWII, start=as.Date("2013-01-01"), end = as.Date("2015-12-31"))
TWII_Train <- cbind(TWII_Subset$TWII.Close - TWII_Subset$TWII.Open)



#################################################################################
# Baum-Welch Algorithm
#################################################################################

library(RHmm)

# I am using RHmm 2.0.3
# Package Located at https://r-forge.r-project.org/R/?group_id=85

hm_model <- HMMFit(obs=TWII_Train, dis="MIXTURE", nStates=5, nMixt=4,control=list(iter=2000))

print(hm_model)

# This is a simplier model with 5 hidden states.
# hm_model <- HMMFit(obs=TWII_Train, nStates=5)


#################################################################################
# Viterbi Algorithm
#################################################################################

VitPath <- viterbi(hm_model, TWII_Train)

#################################################################################
# Predict the known values of the HMM model
#################################################################################

TWII_Predict <- cbind(TWII_Subset$TWII.Close, VitPath$states)

# Scatterplot

chartSeries(TWII_Predict[,1])
addTA(TWII_Predict[TWII_Predict[,2]==1,1],on=1,type="p",col=5,pch=25)
addTA(TWII_Predict[TWII_Predict[,2]==2,1],on=1,type="p",col=6,pch=24)
addTA(TWII_Predict[TWII_Predict[,2]==3,1],on=1,type="p",col=7,pch=23)
addTA(TWII_Predict[TWII_Predict[,2]==4,1],on=1,type="p",col=8,pch=22)
addTA(TWII_Predict[TWII_Predict[,2]==5,1],on=1,type="p",col=10,pch=21) 


#################################################################################
# Here is the calculation of the next observation
#################################################################################

change <- sum(hm_model$HMM$transMat[last(VitPath$states),] * .colSums((matrix(unlist(hm_model$HMM$distribution$mean),nrow=4,ncol=5)) * (matrix(unlist(hm_model$HMM$distribution$proportion), nrow=4,ncol=5)), m=4,n=5))


# select the most recent date from xts object


mydata <- last(TWII_Subset)


predict <- mydata$TWII.Open + change

predict$TWII.Open

# This will give a specific date
# mydata <- TWII_Subset["2014-04-01"]