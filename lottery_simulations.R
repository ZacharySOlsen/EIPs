# Lottery Choice Simulations.
rm(list = ls())

library(tidyverse)
library(LaplacesDemon)
library(randomForest)
library(pdp)

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

# Finding the right cutoff for high vs low dissimilarity.
training_samples = sample.int(nrow(results), 0.75 * nrow(results))
train = results[training_samples,] |> select(EBA_accuracy, EBA_eips, dissimilarity, EV_1, EV_2, var_1, var_2, size) |> ungroup()

EBA_dissim = randomForest(EBA_accuracy ~ dissimilarity, data = train, keep.forest = TRUE)
randomForest::partialPlot(EBA_dissim, pred.data = train, x.var = dissimilarity)
varImpPlot(EBA_dissim)
partial(EBA_dissim, pred.var = "dissimilarity", train = train) |> plotPartial()

small_dissim = results |> filter(dissimilarity < 1)
large_dissim = results |> filter(dissimilarity > 1)

# Initializing vectors to contain names.
graph_name_small = c()
graph_name_large = c()

title_name_large = c()
title_name_small = c()

# For loop creating vector names.
for (i in 2:5) {
  graph = paste("graph", sep = "_", i)
  graph_name_large[i-1] = paste(graph, sep = "_", "large")
  graph_name_small[i-1] = paste(graph, sep = "_", "small")
  
  title_name_large[i-1] = paste(i, sep = " ", "Element Lotteries with Large Dissimilarity")
  title_name_small[i-1] = paste(i, sep = " ", "Element Lotteries with Small Dissimilarity")
}

# Loop creating graphs.
for (i in 2:5) {
  EBA_small = small_dissim |> filter(size == i) |> summarize(mean(EBA_accuracy)) |> pull()
  EBA_eip_small = small_dissim |> filter(size == i) |> summarize(mean(EBA_eips)) |> pull()
  
  EBA_large = large_dissim |> filter(size == i) |> summarize(mean(EBA_accuracy)) |> pull()
  EBA_eip_large = large_dissim |> filter(size == i) |> summarize(mean(EBA_eips)) |> pull()
  
  EQW_small = small_dissim |> filter(size == i) |> summarize(mean(EQW_accuracy)) |> pull()
  EQW_eip_small = small_dissim |> filter(size == i) |> summarize(mean(EQW_eips)) |> pull()
  
  EQW_large = large_dissim |> filter(size == i) |> summarize(mean(EQW_accuracy)) |> pull()
  EQW_eip_large = large_dissim |> filter(size == i) |> summarize(mean(EQW_eips)) |> pull()
  
  LEX_small = small_dissim |> filter(size == i) |> summarize(mean(LEX_accuracy)) |> pull()
  LEX_eip_small = small_dissim |> filter(size == i) |> summarize(mean(LEX_eips)) |> pull()
  
  LEX_large = large_dissim |> filter(size == i) |> summarize(mean(LEX_accuracy)) |> pull()
  LEX_eip_large = large_dissim |> filter(size == i) |> summarize(mean(LEX_eips)) |> pull()
  
  SAT_small = small_dissim |> filter(size == i) |> summarize(mean(SAT_accuracy)) |> pull()
  SAT_eip_small = small_dissim |> filter(size == i) |> summarize(mean(SAT_eips)) |> pull()
  
  SAT_large = large_dissim |> filter(size == i) |> summarize(mean(SAT_accuracy)) |> pull()
  SAT_eip_large = large_dissim |> filter(size == i) |> summarize(mean(SAT_eips)) |> pull()
  
  WADD_accuracy = 1
  WADD_eips = 2 * ( i + i + (i-1)) + 1
  
  name = c("EBA", "EQW", "LEX", "SAT", "WADD")
  
  small_accuracy = c(EBA_small, EQW_small, LEX_small, SAT_small, WADD_accuracy)
  small_eip = c(EBA_eip_small, EQW_eip_small, LEX_eip_small, SAT_eip_small, WADD_eips)
  
  large_accuracy = c(EBA_large, EQW_large, LEX_large, SAT_large, WADD_accuracy)
  large_eip = c(EBA_eip_large, EQW_eip_large, LEX_eip_large, SAT_eip_large, WADD_eips)
  
  graph_data_small = tibble(small_accuracy, small_eip, name)
  graph_data_large = tibble(large_accuracy, large_eip, name)
  
  small_graph = ggplot(data = graph_data_small, mapping = aes(x = small_eip, y = small_accuracy, label = name)) + geom_point() + ylim(0, 1) + geom_text(hjust = 0.4, vjust = -0.2) + labs(title = title_name_small[i-1], x = "Number of EIPs", y = "Accuracy")
  large_graph = ggplot(data = graph_data_large, mapping = aes(x = large_eip, y = large_accuracy, label = name)) + geom_point() + ylim(0, 1) + geom_text(hjust = 0.4, vjust = -0.2) + labs(title = title_name_large[i-1], x = "Number of EIPs", y = "Accuracy")
  
  assign(graph_name_large[i-1], large_graph)
  assign(graph_name_small[i-1], small_graph)
}

# Testing. Not final.
eba_on_dissim = lm(EBA_accuracy ~ dissimilarity + sd_1 + sd_2, data = results) |> summary() |> 
  broom::tidy()

lex_on_dissim = lm(lex_accuracy ~ dissimilarity + sd_1 + sd_2, data = results) |> summary() |>
  broom::tidy()
