# Load necessary library
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# Define sample size
sample_size <- 200

# Generate random sample data
survey_data <- data.frame(
  Age = sample(18:70, sample_size, replace = TRUE),
  Gender = sample(c("Male", "Female", "Non-binary", "Prefer not to say"), sample_size, replace = TRUE),
  Income = round(runif(sample_size, 500, 10000), 2), # Income between $500 and $10,000
  Household_Size = sample(1:6, sample_size, replace = TRUE),
  Overall_Satisfaction = sample(1:5, sample_size, replace = TRUE),
  Education_Level = sample(c("High School", "Undergraduate", "Postgraduate"), sample_size, replace = TRUE),
  Employment_Status = sample(c("Full-time", "Part-time", "Self-employed", "Student", "Unemployed", "Retired"), sample_size, replace = TRUE),
  Marital_Status = sample(c("Single", "Married", "Divorced", "Widowed"), sample_size, replace = TRUE),
  Location = sample(c("Urban", "Suburban", "Rural"), sample_size, replace = TRUE),
  Shopping_Frequency = sample(c("Daily", "Weekly", "Monthly", "Rarely"), sample_size, replace = TRUE),
  Product_Preferences = sample(c("Groceries", "Clothing", "Electronics", "Home Goods"), sample_size, replace = TRUE),
  Shopping_Time = sample(c("Morning", "Afternoon", "Evening", "Weekends"), sample_size, replace = TRUE),
  Brand_Loyalty = sample(c("High", "Moderate", "Low"), sample_size, replace = TRUE),
  Shopping_Channels = sample(c("In-store", "Online", "Both"), sample_size, replace = TRUE),
  Payment_Methods = sample(c("Cash", "Credit Card", "Mobile Payment"), sample_size, replace = TRUE),
  Product_Quality_Importance = sample(1:5, sample_size, replace = TRUE),
  Price_Sensitivity = sample(1:5, sample_size, replace = TRUE),
  Promotions_Discounts_Interest = sample(1:5, sample_size, replace = TRUE),
  Store_Experience = sample(1:5, sample_size, replace = TRUE),
  Customer_Service_Expectations = sample(1:5, sample_size, replace = TRUE),
  Delivery_Pickup_Preferences = sample(c("Home Delivery", "In-store Pickup", "Curbside Pickup"), sample_size, replace = TRUE),
  Product_Satisfaction = sample(1:5, sample_size, replace = TRUE),
  Service_Satisfaction = sample(1:5, sample_size, replace = TRUE),
  Store_Layout_Satisfaction = sample(1:5, sample_size, replace = TRUE),
  Ease_of_Shopping = sample(1:5, sample_size, replace = TRUE),
  Communication_Preferences = sample(c("Email", "SMS", "Social Media"), sample_size, replace = TRUE),
  Social_Media_Engagement = sample(1:5, sample_size, replace = TRUE),
  Membership_Program_Interest = sample(1:5, sample_size, replace = TRUE),
  Shopping_Triggers = sample(c("Advertisements", "Word of Mouth", "Necessity"), sample_size, replace = TRUE),
  Brand_Perception = sample(c("Value for Money", "Quality", "Exclusivity"), sample_size, replace = TRUE),
  Competitor_Awareness = sample(1:5, sample_size, replace = TRUE),
  Online_Shopping_Habits = sample(1:5, sample_size, replace = TRUE),
  App_Usage = sample(1:5, sample_size, replace = TRUE),
  Tech_Savviness = sample(1:5, sample_size, replace = TRUE)
)

# Create Average_Spend as a linear combination of Monthly_Income, Household_Size, and Overall_Satisfaction
survey_data <- survey_data %>%
  mutate(Average_Spend = round(0.05 * Monthly_Income + 20 * Household_Size + 50 * Overall_Satisfaction + rnorm(sample_size, mean = 0, sd = 50), 2))

# Ensure no negative spending
survey_data$Average_Spend <- ifelse(survey_data$Average_Spend < 0, 0, survey_data$Average_Spend)

# Reorder columns to place Average_Spend just after Overall_Satisfaction
survey_data <- survey_data %>%
  dplyr::select(Age, Gender, Monthly_Income, Household_Size, Overall_Satisfaction, Average_Spend, everything())

# Display the dataset
View(survey_data)

# if (requireNamespace("rstudioapi", quietly = TRUE)) {
  current_file_path <- rstudioapi::getActiveDocumentContext()$path
  print(current_file_path)
  # Get the directory of the current file
  current_directory <- dirname(current_file_path)
  print(current_directory)
# }

# Save the dataset to a CSV file in the current working directory
write.csv(survey_data, file.path(current_directory, "customer_survey_data.csv"), row.names = FALSE)
