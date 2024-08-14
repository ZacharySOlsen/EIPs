# Lottery Choice Simulations.
rm(list = ls())

library(tidyverse)
library(LaplacesDemon)

set.seed(1892)

source("lottery_functions.R")

# Parameters.
EBA_cutoff = 50
SAT_cutoff = 20

# For loop simulation
# Setting up lotteries.

results = tibble()

for (j in 2:5) {
  size = j
  for (i in 1:1000) {
    prizes_1 = runif(size, min = 10, max = 100)
    probabilities_1 = prob_generator(size)
    
    lottery_1 = tibble(prizes_1, probabilities_1) |> arrange(desc(probabilities_1)) |>
      rename("prize" = "prizes_1") |> rename("probability" = "probabilities_1")
    
    prizes_2 = runif(size, min = 10, max = 100)
    probabilities_2 = prob_generator(size)
    
    lottery_2 = tibble(prizes_2, probabilities_2) |> arrange(desc(probabilities_2)) |>
      rename("prize" = "prizes_2") |> rename("probability" = "probabilities_2")
    
    
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
    SAT_eips = SAT_EIP(lottery_1 , lottery_2, SAT_cutoff)
    
    # This is down here because I do arrange the lotteries in this function which
    # could bias EIP estimates for SAT.
    lottery_1 = cdf_lottery(lottery_1)
    lottery_2 = cdf_lottery(lottery_2)
    
    # Lottery Dissimilarity Value
    dissimilarity = dissim(lottery_1, lottery_2)
    
    # Binding together
    test = tibble(EV_1, EV_2, var_1, var_2, LEX_accuracy, EBA_accuracy, EQW_accuracy, SAT_accuracy, LEX_eips, EBA_eips, EQW_eips, SAT_eips, dissimilarity, size)
    results = bind_rows(results, test)
  }
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

# Accuracy and EIPs based on dissimilarity amd size.
small_dissim |> filter(size == 2) |> summarize(mean(LEX_accuracy))
large_dissim |> filter(size == 2) |> summarize(mean(LEX_accuracy))

small_dissim |> filter(size == 2) |> summarize(mean(LEX_eips))
large_dissim |> filter(size == 2) |> summarize(mean(LEX_eips))

small_dissim |> filter(size == 2) |> summarize(mean(EBA_accuracy))
large_dissim |> filter(size == 2) |> summarize(mean(EBA_accuracy))

small_dissim |> filter(size == 2) |> summarize(mean(EBA_eips))
large_dissim |> filter(size == 2) |> summarize(mean(EBA_eips))

small_dissim |> filter(size == 2) |> summarize(mean(EQW_accuracy))
large_dissim |> filter(size == 2) |> summarize(mean(EQW_accuracy))

small_dissim |> filter(size == 2) |> summarize(mean(EQW_eips))
large_dissim |> filter(size == 2) |> summarize(mean(EQW_eips))

small_dissim |> filter(size == 2) |> summarize(mean(SAT_accuracy))
large_dissim |> filter(size == 2) |> summarize(mean(SAT_accuracy))

small_dissim |> filter(size == 2) |> summarize(mean(SAT_eips))
large_dissim |> filter(size == 2) |> summarize(mean(SAT_eips))

# Size 3
small_dissim |> filter(size == 3) |> summarize(mean(LEX_accuracy))
large_dissim |> filter(size == 3) |> summarize(mean(LEX_accuracy))

small_dissim |> filter(size == 3) |> summarize(mean(LEX_eips))
large_dissim |> filter(size == 3) |> summarize(mean(LEX_eips))

small_dissim |> filter(size == 3) |> summarize(mean(EBA_accuracy))
large_dissim |> filter(size == 3) |> summarize(mean(EBA_accuracy))

small_dissim |> filter(size == 3) |> summarize(mean(EBA_eips))
large_dissim |> filter(size == 3) |> summarize(mean(EBA_eips))

small_dissim |> filter(size == 3) |> summarize(mean(EQW_accuracy))
large_dissim |> filter(size == 3) |> summarize(mean(EQW_accuracy))

small_dissim |> filter(size == 3) |> summarize(mean(EQW_eips))
large_dissim |> filter(size == 3) |> summarize(mean(EQW_eips))

small_dissim |> filter(size == 3) |> summarize(mean(SAT_accuracy))
large_dissim |> filter(size == 3) |> summarize(mean(SAT_accuracy))

small_dissim |> filter(size == 3) |> summarize(mean(SAT_eips))
large_dissim |> filter(size == 3) |> summarize(mean(SAT_eips))

# Size 4
small_dissim |> filter(size == 4) |> summarize(mean(LEX_accuracy))
large_4_lex_accuracy =large_dissim |> filter(size == 4) |> summarize(mean(LEX_accuracy)) |> pull()

small_dissim |> filter(size == 4) |> summarize(mean(LEX_eips))
large_4_lex_eip =large_dissim |> filter(size == 4) |> summarize(mean(LEX_eips)) |> pull()

small_dissim |> filter(size == 4) |> summarize(mean(EBA_accuracy))
large_4_EBA_accuracy = large_dissim |> filter(size == 4) |> summarize(mean(EBA_accuracy)) |> pull()

small_dissim |> filter(size == 4) |> summarize(mean(EBA_eips))
large_4_EBA_eip = large_dissim |> filter(size == 4) |> summarize(mean(EBA_eips)) |> pull()

small_dissim |> filter(size == 4) |> summarize(mean(EQW_accuracy))
large_4_EQW_accuracy = large_dissim |> filter(size == 4) |> summarize(mean(EQW_accuracy)) |> pull()

small_dissim |> filter(size == 4) |> summarize(mean(EQW_eips))
large_4_EQW_eip = large_dissim |> filter(size == 4) |> summarize(mean(EQW_eips)) |> pull()

small_dissim |> filter(size == 4) |> summarize(mean(SAT_accuracy))
large_4_SAT_accuracy = large_dissim |> filter(size == 4) |> summarize(mean(SAT_accuracy)) |> pull()

small_dissim |> filter(size == 4) |> summarize(mean(SAT_eips))
large_4_SAT_eip = large_dissim |> filter(size == 4) |> summarize(mean(SAT_eips)) |> pull()

WADD_4_EIP = 2*(4 + 4 + 3) + 1

WADD_4_accuracy = 1

accuracy_4 = c(large_4_EBA_accuracy, large_4_EQW_accuracy, large_4_lex_accuracy, large_4_SAT_accuracy, WADD_4_accuracy)
names(accuracy_4) = c("EBA", "EQW", "LEX", "SAT", "WADD")

name = c("EBA", "EQW", "LEX", "SAT", "WADD")

eip_4 = c(large_4_EBA_eip, large_4_EQW_eip, large_4_lex_eip, large_4_SAT_eip, WADD_4_EIP)
names(eip_4) = c("EBA", "EQW", "LEX", "SAT", "WADD")

graph_4_large_data = tibble(accuracy_4, eip_4, name)

ggplot(data = graph_4_large_data ,aes(x = eip_4, y = accuracy_4, label = label)) + geom_point() + geom_text(hjust = 0.4, vjust = -0.2)

# Size 5
small_dissim |> filter(size == 5) |> summarize(mean(LEX_accuracy))
large_dissim |> filter(size == 5) |> summarize(mean(LEX_accuracy))

small_dissim |> filter(size == 5) |> summarize(mean(LEX_eips))
large_dissim |> filter(size == 5) |> summarize(mean(LEX_eips))

small_dissim |> filter(size == 5) |> summarize(mean(EBA_accuracy))
large_dissim |> filter(size == 5) |> summarize(mean(EBA_accuracy))

small_dissim |> filter(size == 5) |> summarize(mean(EBA_eips))
large_dissim |> filter(size == 5) |> summarize(mean(EBA_eips))

small_dissim |> filter(size == 5) |> summarize(mean(EQW_accuracy))
large_dissim |> filter(size == 5) |> summarize(mean(EQW_accuracy))

small_dissim |> filter(size == 5) |> summarize(mean(EQW_eips))
large_dissim |> filter(size == 5) |> summarize(mean(EQW_eips))

small_dissim |> filter(size == 5) |> summarize(mean(SAT_accuracy))
large_dissim |> filter(size == 5) |> summarize(mean(SAT_accuracy))

small_dissim |> filter(size == 5) |> summarize(mean(SAT_eips))
large_dissim |> filter(size == 5) |> summarize(mean(SAT_eips))


results = results |> mutate(sd_1 = sqrt(var_1)) |> mutate(sd_2 = sqrt(var_2))


# Testing. Not final.
eba_on_dissim = lm(EBA_accuracy ~ dissimilarity + sd_1 + sd_2, data = results) |> summary() |> 
  broom::tidy()

lex_on_dissim = lm(lex_accuracy ~ dissimilarity + sd_1 + sd_2, data = results) |> summary() |>
  broom::tidy()
