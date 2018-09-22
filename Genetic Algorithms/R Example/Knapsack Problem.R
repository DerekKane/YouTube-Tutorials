################################################################
# Genetic Algorithm Tutorial
################################################################


# You are going to spend a month in the wilderness. 
# You're taking a backpack with you, however, the maximum weight it can carry is 20 kilograms. 
# You have a number of survival items available, each with its own number of "survival points". 
# You're objective is to maximize the number of survival points.

################################################################
# Load the libraries

library(genalg)
library(ggplot2)
library(animation)

# Create the table for the knapsack problem.

dataset <- data.frame(item = c("pocketknife", "beans", "potatoes", 
                               "unions", "sleeping bag", "rope", "compass"), 
                      survivalpoints = c(10, 20, 15, 2, 30, 10, 30), 
                      weight = c(1, 5, 10, 1, 7, 5, 1))

# Create a variable to represent the maximum weightlimit.

weightlimit <- 20

################################################################

# Before creating the model we have to set-up an evaluation function. 
# The evaluation function will evaluate the different individuals (chromosomes) of the population on the value of their gene configuration.

# An individual can for example have the following gene configuration: 1001100.

# Each number in this binary string represents whether or not to take an item with you. A value of 1 refers to 
# putting the specific item in the knapsack while a 0 refers to leave the item at home. 

################################################################

# Given the example gene configuration we would take the following items;

chromosome = c(1, 0, 0, 1, 1, 0, 0)
dataset[chromosome == 1, ]

# We can check to what amount of surivival points this configuration sums up.

cat(chromosome %*% dataset$survivalpoints)

# Above we gave a value to the gene configuration of a given chromosome. This is exactly what the evaluation function does.

# The genalg algorithm tries to optimize towards the minimum value. 
# Therefore, the value is calculated as above and multiplied with -1. 
# A configuration which leads to exceeding the weight constraint returns a value of 0 (a higher value can also be given).

################################################################
# We define the evaluation function as follows.

evalFunc <- function(x) {
  current_solution_survivalpoints <- x %*% dataset$survivalpoints
  current_solution_weight <- x %*% dataset$weight
  
  if (current_solution_weight > weightlimit) 
    return(0) else return(-current_solution_survivalpoints)
}

################################################################
# Next, we choose the number of iterations, design and run the model.

# install.packages("animation")

iter = 100

GAmodel <- rbga.bin(size = 7, popSize = 200, iters = iter, mutationChance = 0.01, 
                    elitism = T, evalFunc = evalFunc)

cat(summary.rbga(GAmodel))

# The best solution is found to be 1101111. 
# This leads us to take the following items with us on our trip into the wild.

solution = c(1, 1, 0, 1, 1, 1, 1)
dataset[solution == 1, ]

# solution vs available

cat(paste(solution %*% dataset$survivalpoints, "/", sum(dataset$survivalpoints)))

################################################################
# Let's visualize how the model evolves.

animate_plot <- function(x) {
  for (i in seq(1, iter)) {
    temp <- data.frame(Generation = c(seq(1, i), seq(1, i)), Variable = c(rep("mean", 
                                                                              i), rep("best", i)), Survivalpoints = c(-GAmodel$mean[1:i], -GAmodel$best[1:i]))
    
    pl <- ggplot(temp, aes(x = Generation, y = Survivalpoints, group = Variable, 
                           colour = Variable)) + geom_line() + scale_x_continuous(limits = c(0, 
                                                                                             iter)) + scale_y_continuous(limits = c(0, 110)) + geom_hline(y = max(temp$Survivalpoints), 
                                                                                                                                                          lty = 2) + annotate("text", x = 1, y = max(temp$Survivalpoints) + 
                                                                                                                                                                                2, hjust = 0, size = 3, color = "black", label = paste("Best solution:", 
                                                                                                                                                                                                                                       max(temp$Survivalpoints))) + scale_colour_brewer(palette = "Set1") + 
      opts(title = "Evolution Knapsack optimization model")
    
    print(pl)
  }
}

# in order to save the animation

saveMovie(animate_plot(), interval = 0.1, outdir = getwd())



