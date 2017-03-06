print("Problem 1 - Write a loop that calculates 12-factorial")
i <- 12
total <- 12
while (i >= 2)
{
  total <- total * (i - 1)
  i <- i - 1
}
print (total)
print("Problem 2 - Show how to create a numeric vector that contains the sequence from 20 to 50 by 5.")
nvect <- c(seq(20, 50, 5))
print (nvect)
print("Problem 3 - Create the function “factorial” that takes a trio of input numbers a, b, and c and solve the quadratic equation. The function should print as output the two solutions.")
factorial <- function(a, b, c) {
  root <- b ^ 2 - 4 * a * c
  if (root >= 0){
    answer1 = (-b + sqrt(root))/(2 * a)
    answer2 = (-b - sqrt(root))/(2 * a)
    return(c(answer1, answer2))
    }
  else {
    return(NA)
    }
}
print (factorial(1, 5, 1))