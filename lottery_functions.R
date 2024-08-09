# Lottery Choice Simulation Functions.

library(tidyverse)
library(LaplacesDemon)

# Probability generator function. Works for any lottery with more than 2 attributes.
prob_generator = function(num_attributes){
  dir_parameter = rep(1, num_attributes)
  probs = as.vector(rdirichlet(1, alpha = dir_parameter))
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

# Function that calculates the number of EIPs used by the lexicographic rule.
lex_EIPs = function(lottery_1, lottery_2){
  
  # Initializing number of EIPs.
  num_eips = 0
  for (i in 1:length(lottery_1$prize)) {
    comp_1 = lottery_1 |> arrange(desc(probability)) |> select(prize) |> slice(i) |>
      pull()
    comp_2 = lottery_2 |> arrange(desc(probability)) |> select(prize) |> slice(i) |>
      pull()
    
    if(comp_1 > comp_2){
      # Number of EIPs is two reads and one compare for each element of the lottery.
      num_eips = 3*i
      return(num_eips)
    }
    else if(comp_1 < comp_2){
      num_eips = 3*i
      return(num_eips)
    }
  }
  # If lotteries are exactly the same just return one of their expected values.
  num_eips = 3*length(lottery_1$prize)
  return(num_eips)
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

# Function that calculates the number of EIPs used by the EBA rule.
EBA_EIP = function(lottery_1, lottery_2, cutoff){
  
  # Initializing the number of EIPs.
  num_eips = 0
  
  for (i in 1:length(lottery_1$prize)) {
    comp_1 = lottery_1 |> arrange(desc(probability)) |> select(prize) |> slice(i) |>
      pull()
    comp_2 = lottery_2 |> arrange(desc(probability)) |> select(prize) |> slice(i) |>
      pull()
    
    EV_1 = expected_value(lottery_1$prize, lottery_1$probability)
    EV_2 = expected_value(lottery_2$prize, lottery_2$probability)
    
    if( (comp_1 > cutoff) & (comp_2 < cutoff)){
      # Each iteration of the EBA strategy for comparing two lotteries has two reads
      # and two compares.
      num_eips = 4*i
      return(num_eips)
    }
    else if( (comp_1 < cutoff) & (comp_2 > cutoff)){
      num_eips = 4*i
      return(num_eips)
    }
  }
  num_eips = 4*length(lottery_1$prize)
  return(num_eips)
}

# Equal Weighting Function.
EQW = function(lottery_1, lottery_2){
  prize_sum_1 = sum(lottery_1$prize)
  prize_sum_2 = sum(lottery_2$prize)
  
  if(prize_sum_1 > prize_sum_2){
    EV_1 = expected_value(lottery_1$prize, lottery_1$probability)
    return(EV_1)
  }
  else{
    EV_2 = expected_value(lottery_2$prize, lottery_2$probability)
    return(EV_2)
  }
}

# Function calculating EQW EIPs. For m lotteries with n attributes we have
# n*m reads, (n-1)*m ADDS, and m-1 compares.
EQW_EIP = function(num_attributes, num_lotteries){
  num_eips = (num_attributes + (num_attributes - 1)) * num_lotteries + (num_lotteries - 1)
  return(num_eips)
}

# Satisficing Function
SAT = function(lottery_1, lottery_2, cutoff){
  prizes_1 = lottery_1$prize
  prizes_2 = lottery_2$prize
  
  EV_1 = expected_value(lottery_1$prize, lottery_1$probability)
  EV_2 = expected_value(lottery_2$prize, lottery_2$probability)
  # Checking if they pass cutoff value.
  if(all(prizes_1 > cutoff) & ! all(prizes_2 > cutoff)){
    return(EV_1)
  }
  
  else if(all(prizes_2 > cutoff) & ! all(prizes_1 > cutoff)){
    return(EV_2)
  }
  
  else{
    return(random_choice(EV_1, EV_2))
  }
  
}

# Satisificing EIPs. Need to think on this one further. Someone could potentially
# stop doing reads and compares  earlier than this. Currently the above function
# isn't built for that.
SAT_EIP = function(size){
  num_eips = size*2 + size*2
  return(num_eips)
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