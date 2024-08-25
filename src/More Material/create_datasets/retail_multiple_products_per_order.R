library(lubridate)
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# Define branches and their populations
branches <- c("Irbid", "Ajloun", "Jerash", "Mafraq", "Balqa", "Amman", "Zarqa", "Madaba", "Karak", "Tafilah", "Ma'an", "Aqaba")
populations <- c(2050300, 204000, 274500, 637000, 569500, 4642000, 1581000, 219100, 366700, 111500, 183500, 217900)
names(populations) <- branches

# Define Products
products_dataframe <- data.frame(
  "Sequence.Number" = 1:60,
  "Main.Category" = c(
    rep("Health and Beauty", 10),
    rep("Electronic Accessories", 10),
    rep("Home and Lifestyle", 10),
    rep("Sports and Travel", 10),
    rep("Food and Beverages", 10),
    rep("Fashion Accessories", 10)
  ),
  "Sub.Category" = c(
    "Skincare Products", "Hair Care Products", "Makeup and Cosmetics",
    "Fragrances and Perfumes", "Personal Care Appliances", "Dietary Supplements",
    "Men's Grooming Products", "Bath and Body Products", "Oral Care Products",
    "Health and Wellness Devices", "Mobile Phone Accessories", "Computer Accessories", "Audio and Video Accessories", "Camera Accessories", "Wearable Technology", "Charging Accessories", "Gaming Accessories", "Car Electronics", "Home Office Equipment", "Electronic Storage Devices","Home Decor", "Kitchenware", "Bedding and Linens", "Bath Accessories","Furniture", "Gardening Tools and Decor", "Lighting","Storage and Organization", "Home Improvement Tools", "Cleaning Supplies","Athletic Apparel", "Fitness Equipment", "Outdoor Gear", "Sports Accessories","Travel Accessories", "Water Sports Equipment", "Team Sports Equipment","Cycling Accessories", "Winter Sports Gear", "Fitness Trackers and Technology","Snacks and Confectionery", "Beverages", "Dairy Products", "Baked Goods","Canned and Jarred Foods", "Condiments and Sauces", "Grains and Pasta",
    "Health Foods and Organic Products", "Frozen Foods", "Specialty Foods",
    "Jewelry", "Handbags and Wallets", "Watches", "Scarves and Wraps",
    "Hats and Caps", "Belts", "Sunglasses", "Hair Accessories",
    "Ties and Cufflinks", "Gloves and Mittens"
  )
)

# We will generate 500 customers
# Define customer professions and corresponding mean incomes
professions <- c("Student", "Teacher", "Engineer", "Doctor", "Lawyer","Salesperson", "IT Professional", "Manager", "Retired", "Government Employee")
mean_income <- c(5000, 15000, 30000, 40000, 35000, 15000, 30000, 35000, 6000, 9000)
names(mean_income) <- professions

# Set the probabilities, giving "Government Employee" a 40% probability
# Distribute the remaining 60% probability equally among the other 9 professions
profession_weights <- c(rep(0.0666667, 9), 0.4)

# Create a customer dataframe with gender, ID, profession, and income
customer_gender <- c("male", "female")
customer_dataframe <- data.frame(
  customer_id = 1:500,
  gender = sample(customer_gender, 500, replace = TRUE),
  profession = sample(professions, 500, replace = TRUE, prob = profession_weights)
)

# Generate random income for each customer based on their profession
customer_dataframe$income <- sapply(customer_dataframe$profession, function(prof) {
  round(rnorm(1, mean = mean_income[prof], sd = 0.1 * mean_income[prof]),2)
})

# Define payment methods
payment_methods <- c("Visa", "CliQ", "Cash")

# Total number of orders and proportion per branch
total_orders <- 200
orders_per_branch <- round((populations / sum(populations)) * total_orders)

# Function to generate a weighted random date-time
random_datetime <- function(start_date, end_date) {
  years <- year(start_date):year(end_date)
  weights <- seq_along(years)  # Linearly increasing weights
  weights[years == 2020] <- weights[years == 2020] / 2  # Halve the weight for 2020
  weights[years == 2021] <- weights[years == 2021] / 2  # Halve the weight for 2021
  
  selected_year <- sample(years, 1, prob = weights)
  start_of_year <- make_datetime(selected_year, 1, 1)
  end_of_year <- make_datetime(selected_year + 1, 1, 1) - seconds(1)
  
  sec <- as.numeric(difftime(end_of_year, start_of_year, units = "secs"))
  random_date_time <- as.POSIXct(start_of_year + runif(1, min = 0, max = sec), origin = "1970-01-01")
  
  return(random_date_time)
}

# Function to determine the number of products per order based on customer type
get_num_products <- function(is_customer) {
  if (is_customer) {
    max(1, round(rnorm(1, mean = 8, sd = 3)))
  } else {
    max(1, round(rnorm(1, mean = 5, sd = 6)))
  }
}

# Function to generate multiple records of products purchases for a given order
get_products <- function(branch, order_id) {
  is_customer <- sample(c(TRUE, FALSE), 1)
  customer_id <- sample(1:nrow(customer_dataframe), 1)
  customer_profession <- customer_dataframe$profession[customer_id]
  customer_income <- customer_dataframe$income[customer_id]
  customer_gender <- customer_dataframe$gender[customer_id]
  
  num_products <- get_num_products(is_customer)
  
  # For each order, we generate number of products in that list based on the customer status
  # Customers tend to buy more products than non-customers
  products_list <- sample(1:nrow(products_dataframe), size = num_products, replace = FALSE)
  
  df <- data.frame()
  
  for (product in products_list) {
    purchase_datetime <- random_datetime(as.POSIXct("2015-01-01"), Sys.time())
    unit_price <- round(runif(1, min = 5, max = 100),2)
    
    # Generate quantity with the specified correlations
    base_quantity <- rnorm(1, mean = 5, sd = 2)
    quantity <- base_quantity - 0.4 * (unit_price / 10) + 0.3 * (customer_income / 10000) + ifelse(customer_gender == "female", 2, 0)
    quantity <- round(max(1, min(quantity, 10)))  # Ensure quantity is between 1 and 10
    
    total_price <- unit_price * quantity
    tax_amount <- round(total_price * 0.16,2)
    
    if (branch %in% c("Karak", "Tafila", "Ma'an")) {
      customer_gender <- ifelse(runif(1) < 0.7, "male", "female")
    } else if (branch == "Amman") {
      customer_gender <- ifelse(runif(1) < 0.6, "female", "male")
    } else {
      customer_gender <- sample(customer_gender, 1)
    }
    
    payment_method <- sample(payment_methods, 1)
    if (branch %in% c("Amman", "Aqaba") && runif(1) > 0.3) {
      payment_method <- sample(c("Visa", "CliQ"), 1)
    }
    
    record <- c(
      order_id = order_id,
      branch_name = branch,
      is_customer = is_customer,
      customer_id = customer_id,
      customer_profession = customer_profession,
      customer_income = customer_income,
      purchase_date = format(purchase_datetime, "%Y-%m-%d"),  # Ensure proper date format
      purchase_time = format(purchase_datetime, "%H:%M:%S"),
      product_line = product,
      unit_price = unit_price,
      quantity = quantity,
      tax_amount = tax_amount,
      customer_gender = customer_gender,
      payment_method = payment_method
    )
    
    new_row_as_df <- as.data.frame(t(record))
    df <- rbind(df, new_row_as_df)
  }
  return(df)
}

##############################################################
# Entry Point
##############################################################

# Generate data for each branch
retail_multi_products <- data.frame()

print("Start executing...")
for (branch in names(orders_per_branch)) {
  print(paste("Generating records for: ", branch))
  for (order_id in 1:orders_per_branch[branch]) {
    order_products <- get_products(branch, paste(branch, order_id))
    retail_multi_products <- rbind(retail_multi_products, order_products)
  }
}

# Convert the 'purchase_date' column to Date type explicitly
retail_multi_products$purchase_date <- as.Date(retail_multi_products$purchase_date, format = "%Y-%m-%d")

# Display a sample of the dataset
View(retail_multi_products)

if (requireNamespace("rstudioapi", quietly = TRUE)) {
  current_file_path <- rstudioapi::getActiveDocumentContext()$path
  print(current_file_path)
  # Get the directory of the current file
  current_directory <- dirname(current_file_path)
  print(current_directory)
}

# Save the dataset to a CSV file in the current working directory
write.csv(retail_multi_products, file.path(current_directory, "retail_multi_products.csv"), row.names = FALSE)
