
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

# Next lets look at an battle withthe Iron Golem's leader versus the Untamed Leader

# The Iron Golem Leader has 20 Health, 4 toughness, 3 attacks, 5 strength, 2/5 damage.
# The Untamed Leader has 20 Health, 4 toughness, 4 attacks, 4 strength, 2/5 damage.

# In a typical attack action, the Iron Golem leader will roll 3 dice and will need
# a 3, 4 or 5 on a die to do 2 damage as str is greater and 6 on a die to do 5 damage.
ig_attack <- c(0,0,2,2,2,5)
3 * mean(ig_attack) # 5.5 expected damage per attack action

# fuction for monte carlo results
attack_results <- function(str, toughness, attacks, dmg, crit_dmg) {
  
  # first we build the dice table
  if (str > toughness) {
    dice <- c(0,0,dmg,dmg,dmg,crit_dmg)
  }
  if (str == toughness) {
    dice <- c(0,0,0,dmg,dmg,crit_dmg)
  }
  if (str < toughness) {
    dice <- c(0,0,0,0,dmg,crit_dmg)
  }
  
  results <- c()
  for (i in 1:10000) {
    roll <- sample(dice, attacks)
    total <- sum(roll)
    results <- c(results, total)
  }
  table_result <- table(results) / sum(table(results)) * 100
  return(table_result)
}

# The Iron Golem Dominar has 20 Health, 4 toughness, 3 attacks, 5 strength, 2/5 damage.
# The Untamed Heart-Eater has 20 Health, 4 toughness, 4 attacks, 4 strength, 2/5 damage.

ig_attacks <- attack_results(str = 5, toughness = 4, attacks = 3, dmg = 2, crit_dmg = 5)
barplot(ig_attacks, xlab = 'Damage', ylab = 'Percentage', 
        main  = 'Dominar attacks Heart-Eater', ylim = c(0, 50))

# The Iron Golem Dominar has 20 Health, 4 toughness, 3 attacks, 5 strength, 2/5 damage.
# The Untamed Heart-Eater has 20 Health, 4 toughness, 4 attacks, 4 strength, 2/5 damage.

ut_attacks <- attack_results(str = 4, toughness = 4, attacks = 4, dmg = 2, crit_dmg = 5)
barplot(ut_attacks, xlab = 'Damage', ylab = 'Percentage', 
        main  = 'Heart-Eater attacks Dominar', ylim = c(0, 50))

# looking at the random guys, the plains-runners
ut_attacks <- attack_results(str = 3, toughness = 4, attacks = 3, dmg = 1, crit_dmg = 3)
barplot(ut_attacks, xlab = 'Damage', ylab = 'Percentage', 
        main  = 'Plains-runner attacks Dominar', ylim = c(0, 50))


# looking at the prey taker's weapon options, the sawtooth blade or the fanged axe
par( mfrow = c(3,2) )
ut_attacks <- attack_results(attacks = 3, str = 4, dmg = 2, crit_dmg = 4, toughness = 3)
barplot(ut_attacks, xlab = 'Damage', ylab = 'Percentage', 
        main  = 'Prey Taker with Fanged Axe vs Toughness 3', ylim = c(0, 50))
ut_attacks <- attack_results(attacks = 4, str = 3, dmg = 2, crit_dmg = 4, toughness = 3)
barplot(ut_attacks, xlab = 'Damage', ylab = 'Percentage', 
        main  = 'Prey Taker with Sawtooth Blade vs Toughness 3', ylim = c(0, 50))
ut_attacks <- attack_results(attacks = 3, str = 4, dmg = 2, crit_dmg = 4, toughness = 4)
barplot(ut_attacks, xlab = 'Damage', ylab = 'Percentage', 
        main  = 'Prey Taker with Fanged Axe vs Toughness 4', ylim = c(0, 50))
ut_attacks <- attack_results(attacks = 4, str = 3, dmg = 2, crit_dmg = 4, toughness = 4)
barplot(ut_attacks, xlab = 'Damage', ylab = 'Percentage', 
        main  = 'Prey Taker with Sawtooth Blade vs Toughness 4', ylim = c(0, 50))
ut_attacks <- attack_results(attacks = 3, str = 4, dmg = 2, crit_dmg = 4, toughness = 5)
barplot(ut_attacks, xlab = 'Damage', ylab = 'Percentage', 
        main  = 'Prey Taker with Fanged Axe vs Toughness 5', ylim = c(0, 50))
ut_attacks <- attack_results(attacks = 4, str = 3, dmg = 2, crit_dmg = 4, toughness = 5)
barplot(ut_attacks, xlab = 'Damage', ylab = 'Percentage', 
        main  = 'Prey Taker with Sawtooth Blade vs Toughness 5', ylim = c(0, 50))



par( mfrow = c(1,1) )
damage <- attack_results(attacks = 2, str = 6, dmg = 4, crit_dmg = 8, toughness = 4)
barplot(damage, xlab = 'Damage', ylab = 'Percentage', 
        main  = 'Ogor Breacher vs Toughness 5 or less', ylim = c(0, 50))

par( mfrow = c(1,1) )
damage <- attack_results(attacks = 4, str = 4, dmg = 2, crit_dmg = 4, toughness = 4)
barplot(damage, xlab = 'Damage', ylab = 'Percentage', 
        main  = 'Drill Master vs Toughness 4', ylim = c(0, 50))
