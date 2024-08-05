# Lottery Choice Simulations
rm(list = ls())

library(tidyverse)

set.seed(1892)

# Probability generator function. Works for any lottery with more than 2 attributes.
prob_generator = function(num_attributes){
  probs = c(runif(1))
  for (i in 1:(num_attributes - 2)) {
    prob = runif(1, max = 1 - sum(probs))
    probs[i+1] = prob
  }
  probs = append(probs, 1 - sum(probs))
  return(probs)
}


# Expected Value Function.
expected_value = function(lottery, probabilities){
  # Element-wise multiplication of probablities to prizes. Then summed up.
  value = sum(lottery * probabilities)
  return(value)
}

# Lottery Variance Function.
variance = function(lottery, probabilities){
  variance = sum(lottery^2 * probabilities) - expected_value(lottery, probabilities)^2
  return(variance)
}

# Random Choice Function
random_choice = function(EV_1, EV_2){
  # Random choice if both are always above the cutoff.
  roll = rbinom(1, 1, prob = 0.5)
  if(roll == 1){
    return(EV_2)
  }
  
  else{
    return(EV_1)
  }
}
# Lexicographic function. Orders attributes by probability.
lexicographic = function(lottery_1, lottery_2){
  
  for (i in 1:length(lottery_1$prize)) {
    comp_1 = lottery_1 |> arrange(desc(probability)) |> select(prize) |> slice(i) |>
      pull()
    comp_2 = lottery_2 |> arrange(desc(probability)) |> select(prize) |> slice(i) |>
      pull()
    
    if(comp_1 > comp_2){
      return(expected_value(lottery_1$prize, lottery_1$probability))
    }
    else if(comp_1 < comp_2){
      return(expected_value(lottery_2$prize, lottery_2$probability))
    }
  }
  # If lotteries are exactly the same just return one of their expected values.
  return(expected_value(lottery_1$prize, lottery_1$probability))
}

# Pure EBA Function. Orders attributes by probability. Then does comparisons based
# on the cutoff.
EBA = function(lottery_1, lottery_2, cutoff){
  
  for (i in 1:length(lottery_1$prize)) {
    comp_1 = lottery_1 |> arrange(desc(probability)) |> select(prize) |> slice(i) |>
      pull()
    comp_2 = lottery_2 |> arrange(desc(probability)) |> select(prize) |> slice(i) |>
      pull()
    
    EV_1 = expected_value(lottery_1$prize, lottery_1$probability)
    EV_2 = expected_value(lottery_2$prize, lottery_2$probability)
    
    if( (comp_1 > cutoff) & (comp_2 < cutoff)){
      return(EV_1)
    }
    else if( (comp_1 < cutoff) & (comp_2 > cutoff)){
      return(EV_2)
    }
  }
  return(random_choice(EV_1, EV_2))
}

# Function creating the cumulative probabilities
cdf_lottery = function(lottery) {
  lottery = lottery |> arrange(prize) |> 
    mutate(cumulative_prob = cumsum(probability))
  return(lottery)
}

# CDF Function.
cdf = function(lottery, value){
  if(value < min(lottery$prize)){
    return(0)
  }
  else if(value > max(lottery$prize)){
    return(1)
  }
  else{
    return(lottery |> filter(prize <= value) |> 
             summarize(max(cumulative_prob)) |> pull())
  }
}

# Dissimilarity function.

# Basically this function just calculates the area of all the rectangles of
# differences in the two cdfs and adds them up. Then subtract the two expected
# values.
dissim = function(lottery_1, lottery_2){
  
  # Sorting the two lotteries prizes from smallest to largest.
  prizes_1 = pull(lottery_1, prize)
  prizes_2 = pull(lottery_2, prize)
  prizes_all = sort(c(prizes_1, prizes_2))
  
  area = c()
  
  # For loop finds the area of all the rectangles
  for (i in 1:(length(prizes_all) - 1)) {
    base = prizes_all[i+1] - prizes_all[i]
    height = abs(cdf(lottery_1, prizes_all[i]) - cdf(lottery_2, prizes_all[i]))
    area[i] = base * height
  }
  
  # Summing up the areas of the rectangles
  sum_areas = sum(area)
  
  # Calculating expected values for the two lotteries.
  EV_1 = expected_value(lottery_1$prize, lottery_1$probability)
  EV_2 = expected_value(lottery_2$prize, lottery_2$probability)
  
  # Returning the dissimilarity index.
  dissimilarity = sum_areas - abs(EV_1 - EV_2)
  return(dissimilarity)
}

# Relative Accuracy Function for Heuristics.
relative_accuracy = function(choice, EV_1, EV_2){
  score = (choice - 0.5*(EV_1 + EV_2))/(max(EV_1, EV_2) - 0.5*(EV_1 + EV_2))
  return(score)
}


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
