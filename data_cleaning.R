library(dplyr)
library(readr)
library(purrr)

# Load and Merge Data
branch1 <- read_csv("Branch1.csv")
branch2 <- read_csv("Branch2.csv")
branch3 <- read_csv("Branch3.csv")

cd data <- bind_rows(branch1, branch2, branch3)

# Handle Missing Values
numeric_cols <- names(select(data, where(is.numeric)))
data <- data %>% mutate(across(all_of(numeric_cols), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

# Remove Duplicates
data <- distinct(data)

# Detect and Cap Outliers
cap_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  pmax(pmin(x, Q3 + 1.5 * IQR), Q1 - 1.5 * IQR)
}
data <- data %>% mutate(across(all_of(numeric_cols), cap_outliers))

# Assign Correct Branch Names
data <- data %>% mutate(Branch = factor(case_when(
  row_number() <= nrow(branch1) ~ "Branch1",
  row_number() <= nrow(branch1) + nrow(branch2) ~ "Branch2",
  TRUE ~ "Branch3"
)))

# Ensure Left is Binary (0 or 1)
data$Left <- ifelse(data$Left > 1, 1, ifelse(data$Left < 0, 0, data$Left))

# Exclude Customer_ID from summary statistics
numeric_cols_filtered <- setdiff(numeric_cols, "Customer_ID")
branch_stats <- data %>% group_by(Branch) %>% 
  summarise(across(all_of(numeric_cols_filtered), list(mean = mean, sd = sd), na.rm = TRUE))

# Add back Customer_ID Mean and SD separately
customer_id_stats <- data %>% group_by(Branch) %>% 
  summarise(Customer_ID_mean = mean(Customer_ID, na.rm = TRUE), 
            Customer_ID_sd = sd(Customer_ID, na.rm = TRUE))

# Merge stats
global_stats <- left_join(customer_id_stats, branch_stats, by = "Branch")
print(global_stats)

# Save Cleaned Data
# write_csv(data, "Cleaned_Data.csv")

