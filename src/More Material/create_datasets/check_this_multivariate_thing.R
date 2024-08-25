# Code by ChatGPT
# The multi-variate seems interesting

# Load necessary libraries
library(dplyr)
library(MASS)

# Set seed for reproducibility
set.seed(123)

# Define sample size
sample_size <- 200

# Define means for the variables
means <- c(Monthly_Income = 5000, Household_Size = 3, Overall_Satisfaction = 3, Average_Spend = 100)

# Define covariance matrix to introduce stronger correlations
cov_matrix <- matrix(c(
  4000000,  2000,     1500,     50000,  # Monthly_Income
  2000,     4,        2,        50,     # Household_Size
  1500,     2,        4,        50,     # Overall_Satisfaction
  50000,    50,       50,       10000   # Average_Spend
), nrow = 4)

# Generate multivariate normal data
multivariate_data <- mvrnorm(n = sample_size, mu = means, Sigma = cov_matrix)

# Create the data frame
survey_data <- data.frame(
  Monthly_Income = round(multivariate_data[, 1], 2),
  Household_Size = round(multivariate_data[, 2]),
  Overall_Satisfaction = round(multivariate_data[, 3]),
  Average_Spend = round(pmax(multivariate_data[, 4], 0), 2) # Ensure non-negative Average_Spend
)


View(survey_data)
