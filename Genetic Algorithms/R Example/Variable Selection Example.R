######################################################################
# Genetic Algorithm Tutorial - Variable Selection
######################################################################

library("GA")

data("fat", package="UsingR")

mydata <- fat

######################################################################
# Build a Regression Model with all of the variables.
######################################################################

# build a linear regression model with all variables

model <- lm(body.fat.siri ~ age + weight + height + neck + chest + abdomen
          + hip + thigh + knee + ankle + bicep + forearm + wrist, data=mydata)

summary(model)

######################################################################
# Create the Genetic Algorithm
######################################################################

# The design matrix without the intercept is extracted from the 
# fitted model object by:

x <- model.matrix(model)[,-1]
y <- model.response(model.frame(model))

# Then the fitness function can be maximized by the following:

fitness <- function(string){
  inc <- which(string == 1)
  X <- cbind(1, x[,inc])
  mod <- lm.fit(X,y)
  class(mod) <- "lm"
  -AIC(mod)
}

# This function estimates the regression model using predictors
# identified by a 1 in the corresponding position of the string.


# Now lets run the genetic algorithm

GA <- ga("binary", fitness = fitness, nBits = ncol(x), names= colnames(x), monitor=plot)

plot(GA)
summary(GA)

######################################################################
# Fit a regression model based upon the GA results.
######################################################################

# This section will now create a linear regression fit based upon the GA model.

model2 <- lm(body.fat.siri~., data=data.frame(body.fat.siri = y, x[,GA@solution == 1]))

summary(model2)