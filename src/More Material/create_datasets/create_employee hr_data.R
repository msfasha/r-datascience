# Load necessary libraries
library(dplyr)
library(lubridate)

# Set seed for reproducibility
set.seed(123)

# Hardcoded population data for 12 main counties (governorates) in Jordan
population_data <- data.frame(
  City = c("Amman", "Zarqa", "Irbid", "Ajloun", "Aqaba", "Balqa", "Jerash", "Karak", 
           "Ma'an", "Madaba", "Mafraq", "Tafilah"),
  Population = c(4007526, 635160, 502714, 176080, 188160, 491709, 237059, 316629, 
                 144082, 189192, 549948, 104000)
)

# Calculate the proportion of employees for each branch based on population
total_population <- sum(population_data$Population)
population_data <- population_data %>%
  mutate(EmployeeProportion = Population / total_population)

# Adjust the proportions for Jerash and Karak to assign fewer employees
population_data <- population_data %>%
  mutate(EmployeeProportion = case_when(
    City == "Jerash" ~ EmployeeProportion * 0.5,  # Halve the proportion for Jerash
    City == "Karak" ~ EmployeeProportion * 0.5,   # Halve the proportion for Karak
    TRUE ~ EmployeeProportion
  ))

# Normalize the proportions to sum to 1 after adjustment
population_data <- population_data %>%
  mutate(EmployeeProportion = EmployeeProportion / sum(EmployeeProportion))

# Generate synthetic data
n <- 1000

# Define departments with their respective ratios
departments <- c("Sales", "Marketing", "IT", "Tellers", "Customer Service", "HR", "Finance and Accounting", 
                 "Risk Management", "Loan Department", "Operations")
department_prob <- c(0.20, 0.10, 0.05, 0.15, 0.15, 0.05, 0.10, 0.05, 0.10, 0.05)

# Define job titles for each department
job_titles <- list(
  "Sales" = c("Sales Representative", "Relationship Manager", "Sales Manager", "Client Advisor", "Business Development Manager"),
  "Marketing" = c("Marketing Specialist", "Marketing Manager", "Digital Marketing Coordinator", "Brand Manager", "Social Media Strategist"),
  "IT" = c("IT Support Specialist", "Software Developer", "IT Manager", "Systems Analyst", "Network Administrator", "Cybersecurity Analyst", "Database Administrator"),
  "Tellers" = c("Teller", "Senior Teller", "Teller Supervisor", "Head Teller"),
  "Customer Service" = c("Customer Service Representative", "Customer Support Specialist", "Call Center Agent", "Customer Service Manager", "Client Relations Specialist"),
  "HR" = c("HR Specialist", "HR Manager", "Recruiter", "HR Coordinator", "Training and Development Manager", "Compensation and Benefits Manager"),
  "Finance and Accounting" = c("Accountant", "Financial Analyst", "Accounting Manager", "Payroll Specialist", "Chief Financial Officer (CFO)", "Tax Specialist"),
  "Risk Management" = c("Risk Analyst", "Risk Manager", "Compliance Officer", "Internal Auditor", "Fraud Investigator"),
  "Loan Department" = c("Loan Officer", "Loan Processor", "Loan Underwriter", "Loan Servicing Specialist", "Mortgage Advisor"),
  "Operations" = c("Operations Manager", "Branch Operations Coordinator", "Operations Analyst", "Branch Manager", "Logistics Coordinator")
)

# Define average and standard deviation of job satisfaction for each county
satisfaction_params <- data.frame(
  City = population_data$City,
  MeanSatisfaction = c(7.8, 6.5, 8.0, 7.0, 8.5, 6.8, 4.9, 6.3, 3.3, 7.6, 6.7, 7.2),
  SdSatisfaction = c(1.0, 1.5, 0.8, 1.2, 0.5, 1.4, 0.9, 1.6, 0.6, 1.1, 1.5, 1.3)
)

# Rename City to Branch for the join operation
satisfaction_params <- satisfaction_params %>%
  rename(Branch = City)

# Generate employees data
employee_data <- data.frame(
  # Unique identifier for each employee
  EmployeeID = 1:n,
  # Age of the employee
  Age = sample(22:60, n, replace = TRUE),
  # Branch or location where the employee works, proportional to adjusted population
  Branch = sample(population_data$City, n, replace = TRUE, prob = population_data$EmployeeProportion)
)

# Assign gender based on branch
employee_data <- employee_data %>%
  mutate(
    Gender = case_when(
      Branch %in% c("Karak", "Ma'an", "Tafilah") ~ sample(c("M", "F"), n(), replace = TRUE, prob = c(0.80, 0.20)),
      Branch == "Amman" ~ sample(c("F", "M"), n(), replace = TRUE, prob = c(0.60, 0.40)),
      TRUE ~ sample(c("M", "F"), n(), replace = TRUE, prob = c(0.50, 0.50))
    )
  )

# Assign departments and job titles based on branch
employee_data <- employee_data %>%
  group_by(Branch) %>%
  mutate(
    # Department in which the employee works
    Department = sample(departments, n(), replace = TRUE, prob = department_prob),
    # Job title or position of the employee
    JobTitle = mapply(function(department) {
      sample(job_titles[[department]], 1)
    }, Department)
  ) %>%
  ungroup() %>%
  mutate(
    # Salary of the employee based on department
    Salary = case_when(
      Department == "IT" ~ round(runif(n(), 60000, 120000), 2),
      Department == "Sales" ~ round(runif(n(), 30000, 100000), 2),
      Department == "Marketing" ~ round(runif(n(), 40000, 90000), 2),
      Department == "Tellers" ~ round(runif(n(), 25000, 50000), 2),
      Department == "Customer Service" ~ round(runif(n(), 30000, 60000), 2),
      Department == "HR" ~ round(runif(n(), 40000, 80000), 2),
      Department == "Finance and Accounting" ~ round(runif(n(), 50000, 100000), 2),
      Department == "Risk Management" ~ round(runif(n(), 50000, 100000), 2),
      Department == "Loan Department" ~ round(runif(n(), 40000, 80000), 2),
      Department == "Operations" ~ round(runif(n(), 40000, 90000), 2),
      TRUE ~ round(runif(n(), 30000, 100000), 2)
    ),
    # Number of years the employee has been working
    YearsOfExperience = round(runif(n(), 1, 35), 2),
    # Performance rating of the employee
    PerformanceRating = round(runif(n(), 1, 5), 2),
    # Date when the employee was hired
    DateOfHire = sample(seq(as.Date('2000/01/01'), as.Date('2022/01/01'), by="day"), n, replace = TRUE),
    # Date when the employee left the company (if applicable)
    DateOfLeave = sample(c(NA, sample(seq(as.Date('2000/01/01'), as.Date('2023/01/01'), by="day"), n/10, replace = TRUE)), n, replace = TRUE),
    # Education level of the employee with updated probabilities
    EducationLevel = sample(c("Bachelor's", "Master's", "PhD"), n, replace = TRUE, prob = c(0.90, 0.07, 0.03)),
    # Number of training hours completed by the employee
    TrainingHours = sample(0:100, n, replace = TRUE),
    # Number of projects completed by the employee
    ProjectsCompleted = sample(0:20, n, replace = TRUE),
    # Score of the last performance review
    LastPerformanceReviewScore = round(runif(n(), 1, 10), 2),
    # Number of sick leave days taken
    SickLeaveDays = sample(0:15, n, replace = TRUE),
    # Average number of hours worked per month
    AverageMonthlyHours = round(runif(n(), 150, 250), 2),
    # Frequency of working from home
    WorkFromHomeFrequency = sample(0:20, n, replace = TRUE),
    # Number of projects the employee is involved in
    ProjectsInvolvement = sample(1:10, n, replace = TRUE),
    # Employee engagement score
    EmployeeEngagementScore = round(runif(n(), 1, 100), 0)
  ) %>%
  left_join(satisfaction_params, by = "Branch") %>%
  mutate(
    # Job satisfaction score based on a normal distribution specific to each branch
    JobSatisfaction = round(rnorm(n, MeanSatisfaction, SdSatisfaction)),
    JobSatisfaction = ifelse(JobSatisfaction < 1, 1, ifelse(JobSatisfaction > 10, 10, JobSatisfaction)),
    # Tenure of the employee in years
    Tenure = round(as.numeric(difftime(Sys.Date(), DateOfHire, units = "weeks")) / 52, 2)
  )

# Sample late attendance and early leaving times
employee_data <- employee_data %>%
  mutate(
    AverageLateAttending = round(rexp(n, rate = 1/3), 2), # Exponential distribution with mean of 3 minutes
    AverageEarlyLeaving = round(rexp(n, rate = 1/5), 2)  # Exponential distribution with mean of 5 minutes
  )

# Ensure times are within reasonable limits (0 to max late/early)
employee_data$AverageLateAttending <- ifelse(employee_data$AverageLateAttending > 60, 60, employee_data$AverageLateAttending)
employee_data$AverageEarlyLeaving <- ifelse(employee_data$AverageEarlyLeaving > 60, 60, employee_data$AverageEarlyLeaving)

# Save the dataset to a CSV file
if (requireNamespace("rstudioapi", quietly = TRUE)) {
  current_file_path <- rstudioapi::getActiveDocumentContext()$path
  print(current_file_path)
  # Get the directory of the current file
  current_directory <- dirname(current_file_path)
  print(current_directory)
}

# Save the dataset to a CSV file in the current working directory
write.csv(employee_data, file.path(current_directory, "employee_data.csv"), row.names = FALSE)


