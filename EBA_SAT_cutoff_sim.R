# Testing for EBA and SAT cutoffs.
rm(list = ls())

# Packages
library(tidyverse)
library(LaplacesDemon)

# Sourcing from lottery functions file.
source("lottery_functions.R")

# Setting seed.
set.seed(0179)

# Initializing Parameter plus tibble.
size = 5
final_results = tibble()

# For loop changing the cutoff from 10 to 100
for (j in 1:10){
  
  # Cutoff value
  cutoff = 10*j
  
  # Initializing table
  results = tibble()
  
  # For loops generating 100 lotteries and evaluating them using this cutoff.
  for (i in 1:300) {
    
    # Prizes and Probabilities for lottery 1.
    prizes_1 = runif(size, min = 10, max = 100)
    probabilities_1 = prob_generator(size)
    
    # Putting lottery 1 together and renaming columns.
    lottery_1 = tibble(prizes_1, probabilities_1) |> arrange(desc(probabilities_1)) |>
      rename("prize" = "prizes_1") |> rename("probability" = "probabilities_1")
    
    # Prizes and Probabilities for lottery 2.
    prizes_2 = runif(size, min = 10, max = 100)
    probabilities_2 = prob_generator(size)
    
    # Putting lottery 1 together and renaming columns
    lottery_2 = tibble(prizes_2, probabilities_2) |> arrange(desc(probabilities_2)) |>
      rename("prize" = "prizes_2") |> rename("probability" = "probabilities_2")
    
    # Adding CDF values to the tibble.
    lottery_1 = cdf_lottery(lottery_1)
    lottery_2 = cdf_lottery(lottery_2)
    
    # Expected value for the two lotteries.
    EV_1 = expected_value(lottery_1$prize, lottery_1$probability)
    var_1 = variance(lottery_1$prize, lottery_1$probability)
    
    # Variance of the two lotteries.
    EV_2 = expected_value(lottery_2$prize, lottery_2$probability)
    var_2 = variance(lottery_2$prize, lottery_2$probability)
    
    # EBA Choice
    EBA_choice = EBA(lottery_1, lottery_2, cutoff)
    EBA_accuracy = relative_accuracy(EBA_choice, EV_1, EV_2)
    EBA_eips = EBA_EIP(lottery_1, lottery_2, cutoff)
    
    # SAT Choice
    SAT_Choice = SAT(lottery_1, lottery_2, cutoff)
    SAT_accuracy = relative_accuracy(SAT_Choice, EV_1, EV_2)
    SAT_eips = SAT_EIP(size)
    
    # Binding together
    round_variables = tibble(EBA_accuracy, EBA_eips, SAT_accuracy, SAT_eips)
    results = bind_rows(results, round_variables)
}
  
  # Cutoff used for these observations
  round_cutoff = cutoff
  
  # Average results for the EBA and Lexicographic Rule.
  EBA_average = results |> summarize(mean(EBA_accuracy))
  SAT_average = results |> summarize(mean(SAT_accuracy))
  averages = tibble(round_cutoff, EBA_average, SAT_average)
  final_results = bind_rows(final_results, averages)
}

# Seems 20 is best for SAT. 50 or 60 for the EBA cutoff.