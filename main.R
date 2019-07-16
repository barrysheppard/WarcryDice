
# Dice Calculator for Warcry
# 6 Dice are rolled each turn, then we count the number of uniques, doubles, triples, and quads

#######################################
# Normal prep code                    #
#######################################

# This clears the workspace environment
rm(list = ls())
# This sets the working directory to the same as the file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# This installs all the packages needed if not already loaded
if (!require('pacman')) install.packages('pacman')
pacman::p_load('psych')


# Start with an empty df
dice_rolls <- data.frame(1, 2, 3, 4, 5, 6)
# blank the array
dice_rolls <- dice_rolls[-1,]

# lets roll some dice!
n <- 10000
for(i in 1:n){
  # lets make a random array of dice
  pool <- sample(6, 6, replace = TRUE)
  # count the number of each dice
  count1 <- sum(pool == 1)
  count2 <- sum(pool == 2)
  count3 <- sum(pool == 3)
  count4 <- sum(pool == 4)
  count5 <- sum(pool == 5)
  count6 <- sum(pool == 6)
  # group it up
  groups <- c(count1, count2, count3, count4, count5, count6)
  # count the number of singls, doubles, triples, etc
  singles <- sum(groups == 1)
  doubles <- sum(groups == 2)
  triples <- sum(groups == 3)
  quads <- sum(groups == 4)
  quint <- sum(groups == 5)
  sext <- sum(groups == 6)
  # group it up
  dist <- c(singles, doubles, triples, quads, quint, sext)
  # add to to the output
  dice_rolls <- rbind(dice_rolls, dist)
}
names(dice_rolls) <- c('Single', 'Double', 'Triple', 'Quad', 'Quint', 'Sext')

describe(dice_rolls)
barplot(dice_rolls)
describe(dice_rolls)
names(dice_rolls)

plot_dist <- c(mean(dice_rolls$Single),
               mean(dice_rolls$Double),
               mean(dice_rolls$Triple),
               mean(dice_rolls$Quad),
               mean(dice_rolls$Quint),
               mean(dice_rolls$Sext)
)

plot(plot_dist, type = 's', xlab = 'Number of dice in each group',
     ylab = 'Average per roll of 6')

plot_percentages <- c(sum(dice_rolls$Single > 0)/100,
                      sum(dice_rolls$Double > 0)/100,
                      sum(dice_rolls$Triple > 0)/100,
                      sum(dice_rolls$Quad > 0)/100,
                      sum(dice_rolls$Quint > 0)/100,
                      sum(dice_rolls$Sext > 0)/100)
names(plot_percentages) <- c('Single', 'Double', 'Triple', 'Quad', 'Quint', 'Sext')

plot(plot_percentages, type = 's', xlab = 'At least one group of this many dice',
     ylab = '% chance of each result', main = 'Warcry Initiative and Ability Dice')
?plot



