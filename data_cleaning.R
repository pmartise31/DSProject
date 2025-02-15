library(dplyr)
library(readr)
library(purrr)
library(ggplot2)

path1 <- 'Branch1.csv'
path2 <- 'Branch2.csv'
path3 <- 'Branch3.csv'

# Load and Merge Data
branch1 <- read_csv(path1)
branch2 <- read_csv(path2)
branch3 <- read_csv(path3)

data <- bind_rows(branch1, branch2, branch3)
# How many NAs in the dataset??
print(sum(is.na(data)))

# Assign Correct Branch Names
data <- data %>% mutate(Branch = factor(case_when(
  row_number() <= nrow(branch1) ~ "Branch1",
  row_number() <= nrow(branch1) + nrow(branch2) ~ "Branch2",
  TRUE ~ "Branch3"
)))

# Handle Missing Values
numeric_cols <- names(select(data, where(is.numeric)))
data <- data %>% mutate(across(all_of(numeric_cols), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

# Remove Duplicates
# 150 duplicates
data <- distinct(data)

# Detect and Cap Outliers --> apply or not?? NO
# cap_outliers <- function(x) {
#   Q1 <- quantile(x, 0.25, na.rm = TRUE)
#   Q3 <- quantile(x, 0.75, na.rm = TRUE)
#   IQR <- Q3 - Q1
#   pmax(pmin(x, Q3 + 1.5 * IQR), Q1 - 1.5 * IQR)
# }

# This ggplots to see the distribution of the variables and see ERRORS
ggplot(data = data, aes(x = Branch, y = Age, fill = Branch))+
  geom_boxplot(fill = "blue", color = "black")+
  labs(title = "Age distribution for every branch", x = "Branches", y = "Frequencies")+
  theme(plot.title = element_text(hjust = 0.5))


ggplot(data = data, aes(x = Branch, y = Score, fill = Branch))+
  geom_boxplot(fill = "blue", color = "black")+
  labs(title = "Score distribution for every branch", x = "Branches", y = "Frequencies")+
  theme(plot.title = element_text(hjust = 0.5))

## 4 outliers are not in the defined range for the score [350-850] --> must be deleted

# We find the indexes where this problematic values are located
indices <- which(data$Score < 350 | data$Score > 850)
print(indices)

# Mean value without outliers
mean_score <- mean(data$Score[-indices])

# Replace the outliers by the mean value
data$Score[indices] <- mean_score

# Boxplot to see if the problem is solved
ggplot(data = data, aes(x = Branch, y = Score, fill = Branch))+
  geom_boxplot(fill = "blue", color = "black")+
  labs(title = "Score distribution for every branch", x = "Branches", y = "Frequencies")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = data, aes(x = Branch, y = Tenure, fill = Branch))+
  geom_boxplot(fill = "blue", color = "black")+
  labs(title = "Tenure distribution for every branch", x = "Branches", y = "Frequencies")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = data, aes(x = Branch, y = Salary, fill = Branch))+
  geom_boxplot(fill = "blue", color = "black")+
  labs(title = "Salary distribution for every branch", x = "Branches", y = "Frequencies")+
  theme(plot.title = element_text(hjust = 0.5))

# Problems 
# Looking for the indices of the outliers
indices = which(data$Salary > 2.5e20)
print(indices)
# Compute the mean without the extreme unreal value (1e21)
mean_salary <- mean(data$Salary[-indices])
# Replace the outliers with the salary value
data$Salary[indices] <- mean_salary

ggplot(data = data, aes(x = Branch, y = Salary, fill = Branch))+
  geom_boxplot(fill = "blue", color = "black")+
  labs(title = "Salary distribution for every branch", x = "Branches", y = "Frequencies")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = data, aes(x = Branch, y = Balance, fill = Branch))+
  geom_boxplot(fill = "blue", color = "black")+
  labs(title = "Balance distribution for every branch", x = "Branches", y = "Frequencies")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = data, aes(x = Branch, y = Products_in_Use, fill = Branch))+
  geom_boxplot(fill = "blue", color = "black")+
  labs(title = "Products in Use distribution for every branch", x = "Branches", y = "Frequencies")+
  theme(plot.title = element_text(hjust = 0.5))

# data <- data %>% mutate(across(all_of(numeric_cols), cap_outliers))

# Ensure Left is Binary (0 or 1)
data$Left <- ifelse(data$Left > 1, 1, ifelse(data$Left < 0, 0, data$Left))

# Exclude Customer_ID from summary statistics

# Find the duplicated Customer_IDs
duplicates <- data$Customer_ID[duplicated(data$Customer_ID)]

# Computing the mean and the standard deviation   
branch_stats <- data %>%
  select(-Customer_ID) %>%  # Exclude 'Customer_ID' column
  group_by(Branch) %>%
  summarise(across(where(is.numeric), list(mean = mean, sd = sd), na.rm = TRUE))

# Add back Customer_ID Mean and SD separately
customer_id_stats <- data %>% group_by(Branch) %>% 
  summarise(Customer_ID_mean = mean(Customer_ID, na.rm = TRUE), 
            Customer_ID_sd = sd(Customer_ID, na.rm = TRUE))

# Merge stats
global_stats <- left_join(customer_id_stats, branch_stats, by = "Branch")
print(global_stats)

# Save Cleaned Data
# write_csv(data, "Cleaned_Data.csv")