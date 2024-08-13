# Lottery Choice Simulations.
rm(list = ls())

library(tidyverse)
library(LaplacesDemon)

set.seed(1892)

source("lottery_functions.R")

# Parameters.
size = 5
EBA_cutoff = 50
SAT_cutoff = 20

# For loop simulation
# Setting up lotteries.

results = tibble()

for (i in 1:300) {
  prizes_1 = runif(size, min = 10, max = 100)
  probabilities_1 = prob_generator(size)
  
  lottery_1 = tibble(prizes_1, probabilities_1) |> arrange(desc(probabilities_1)) |>
    rename("prize" = "prizes_1") |> rename("probability" = "probabilities_1")
  
  prizes_2 = runif(size, min = 10, max = 100)
  probabilities_2 = prob_generator(size)
  
  lottery_2 = tibble(prizes_2, probabilities_2) |> arrange(desc(probabilities_2)) |>
    rename("prize" = "prizes_2") |> rename("probability" = "probabilities_2")
  
  lottery_1 = cdf_lottery(lottery_1)
  lottery_2 = cdf_lottery(lottery_2)
  
  
  EV_1 = expected_value(lottery_1$prize, lottery_1$probability)
  var_1 = variance(lottery_1$prize, lottery_1$probability)
  
  EV_2 = expected_value(lottery_2$prize, lottery_2$probability)
  var_2 = variance(lottery_2$prize, lottery_2$probability)
  
  # Lexicographic Choice
  LEX_choice = lexicographic(lottery_1, lottery_2)
  LEX_accuracy = relative_accuracy(LEX_choice, EV_1, EV_2)
  LEX_eips = LEX_EIP(lottery_1, lottery_2)

  # EBA Choice
  EBA_choice = EBA(lottery_1, lottery_2, EBA_cutoff)
  EBA_accuracy = relative_accuracy(EBA_choice, EV_1, EV_2)
  EBA_eips = EBA_EIP(lottery_1, lottery_2, EBA_cutoff)
  
  # EQW Choice
  EQW_choice = EQW(lottery_1, lottery_2)
  EQW_accuracy = relative_accuracy(EQW_choice, EV_1, EV_2)
  EQW_eips = EQW_EIP(num_attributes = size, num_lotteries = 2)
  
  # SAT Choice
  SAT_choice = SAT(lottery_1, lottery_2, SAT_cutoff)
  SAT_accuracy = relative_accuracy(SAT_choice, EV_1, EV_2)
  SAT_eips = SAT_EIP(size)

  # Lottery Dissimilarity Value
  dissimilarity = dissim(lottery_1, lottery_2)

  # Binding together
  test = tibble(EV_1, EV_2, var_1, var_2, LEX_accuracy, EBA_accuracy, EQW_accuracy, SAT_accuracy, LEX_eips, EBA_eips, EQW_eips, SAT_eips, dissimilarity)
  results = bind_rows(results, test)
}

results |> summarize(mean(LEX_accuracy))
results |> summarize(mean(EBA_accuracy))
results |> summarize(mean(EQW_accuracy))
results |> summarize(mean(SAT_accuracy))

results |> summarize(mean(LEX_eips))
results |> summarize(mean(EBA_eips))
results |> summarize(mean(EQW_eips))
results |> summarize(mean(SAT_eips))

small_dissim = results |> filter(dissimilarity < 1)
large_dissim = results |> filter(dissimilarity > 1)

small_dissim |> summarize(mean(LEX_accuracy))
large_dissim |> summarize(mean(LEX_accuracy))

small_dissim |> summarize(mean(LEX_eips))
large_dissim |> summarize(mean(LEX_eips))

small_dissim |> summarize(mean(EBA_accuracy))
large_dissim |> summarize(mean(EBA_accuracy))

small_dissim |> summarize(mean(EBA_eips))
large_dissim |> summarize(mean(EBA_eips))

small_dissim |> summarize(mean(EQW_accuracy))
large_dissim |> summarize(mean(EQW_accuracy))

small_dissim |> summarize(mean(EQW_eips))
large_dissim |> summarize(mean(EQW_eips))

small_dissim |> summarize(mean(SAT_accuracy))
large_dissim |> summarize(mean(SAT_accuracy))

small_dissim |> summarize(mean(SAT_eips))
large_dissim |> summarize(mean(SAT_eips))


results = results |> mutate(sd_1 = sqrt(var_1)) |> mutate(sd_2 = sqrt(var_2))


# Testing. Not final.
eba_on_dissim = lm(EBA_accuracy ~ dissimilarity + sd_1 + sd_2, data = results) |> summary() |> 
  broom::tidy()

lex_on_dissim = lm(lex_accuracy ~ dissimilarity + sd_1 + sd_2, data = results) |> summary() |>
  broom::tidy()
