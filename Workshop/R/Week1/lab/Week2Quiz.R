# Week2Quiz.R
# Write two function that take a numeric vector as input.
# The first function should return the mean.
# The second function should return the median.

# Do not use the built-in mean() or median() functions!

# built in functions that were used - is.na(), sort()

# remember to write "scaffolding" code that creates test data then
# invokes your functions!

# as a strategy for coding the functions, first write (and test to get it working) 
# the simple case without NAs,and only then add the functionality to 
# gracefully handle missing data

calc_mean = function(vec) {
  # find the mean of a vector
  total <- 0
  n <- 0
  for (i in 1:length(vec)) {
    if (!is.na(vec[i])) {
      total <- total + vec[i]
      n <- n +1
    }
    avg <- total / n  
  }
  return(avg)
  
}

calc_median = function(vec) {
  # find the median of a vector
  
  # you don't have to write your own sort routine
  vec <- sort(vec) # eliminates NAs as a side effect
  
  if (length(vec) == 1) {
    med <- vec[1]  
  }
  else if (length(vec) %% 2 == 1) {
    # if odd number then pick the middle value
    med <- vec[ceiling(length(vec)/2)] # round up
  } 
  else {
    # if even compute average of middle 2 numbers
    n <- ceiling(length(vec)/2)
    med <- (vec[n] + vec[n+1]) /2 
  }
  
  return (med)  
}

# run example 
# test-driven development:  ALWAYS try out first with some simple, easily verifiable datasets
#vec <- c(1,2,4,5,8)
#vec <- c(1,2,4,5,5,8)
#vec <- c(1,2,4,5,8, NA)
vec <- c(-10:29, NA)

vec
calc_mean(vec)
calc_median(vec)
