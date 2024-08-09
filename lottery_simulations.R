# Lottery Choice Simulations.
rm(list = ls())

library(tidyverse)
library(LaplacesDemon)

set.seed(1892)

source("lottery_functions.R")
# Parameters.
size = 5
cutoff = 50

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
  lex_choice = lexicographic(lottery_1, lottery_2)
  lex_accuracy = relative_accuracy(lex_choice, EV_1, EV_2)

  # EBA Choice
  EBA_choice = EBA(lottery_1, lottery_2, cutoff)
  EBA_accuracy = relative_accuracy(EBA_choice, EV_1, EV_2)

  # Lottery Dissimilarity Value
  dissimilarity = dissim(lottery_1, lottery_2)

  # Binding together
  test = tibble(EV_1, EV_2, var_1, var_2, lex_accuracy, EBA_accuracy, dissimilarity)
  results = bind_rows(results, test)
}

results |> summarize(mean(lex_accuracy))
results |> summarize(mean(EBA_accuracy))

small_dissim = results |> filter(dissimilarity < 1)
large_dissim = results |> filter(dissimilarity > 1)

small_dissim |> summarize(mean(lex_accuracy))
large_dissim |> summarize(mean(lex_accuracy))

small_dissim |> summarize(mean(EBA_accuracy))
large_dissim |> summarize(mean(EBA_accuracy))


results = results |> mutate(sd_1 = sqrt(var_1)) |> mutate(sd_2 = sqrt(var_2))


# Testing. Not final.
eba_on_dissim = lm(EBA_accuracy ~ dissimilarity + sd_1 + sd_2, data = results) |> summary() |> 
  broom::tidy()

lex_on_dissim = lm(lex_accuracy ~ dissimilarity + sd_1 + sd_2, data = results) |> summary() |>
  broom::tidy()
