########### Exploratory Data Analysis and Visualization ###########

# All the libraries: 

library(dplyr)
library(readr)
library(purrr)
library(ggplot2)
library(caret)
# install.packages("psych")
library(psych)
# install.packages("corrplot")
library(corrplot)
library(tidyverse)
# install.packages("factoextra")
library(factoextra)
# install.packages("Information")
library(Information)
library(plotrix)
library(cluster)


# Relative path 
relative_path <- "Cleaned_Data.csv"
# Loading the clean data
cleaned_data <- read.csv(relative_path)

## Descriptive statistics
summary(cleaned_data)

#attach(cleaned_data)

# UNIVARIATE ANALYSIS 

# This analysis focuses on exploring the distributions and explore the patterns
# in single features. 

# Names of the columns in the dataframe 
columns <- colnames(cleaned_data)

# Exploring the distribution of the continuous variables using histograms

# For the Age 
ggplot(data = cleaned_data, aes(x= Age))+ 
  geom_histogram(binwidth = 4, fill = "blue", color= "black", alpha = 0.6)+ 
  labs(title = " Age distribution in the banks", x = "Age", y = "Frequency")+ 
  theme(plot.title= element_text(hjust = 0.5))



# For the Score 
ggplot(data = cleaned_data, aes(x= Score))+ 
  geom_histogram(binwidth = 100, fill = "blue", color= "black", alpha = 0.6)+ 
  labs(title = " Score distribution in the banks", x = "Score", y = "Frequency")+
  theme(plot.title= element_text(hjust = 0.5))

# For the Tenure
ggplot(data = cleaned_data, aes(x= Tenure))+ 
  geom_histogram(binwidth = 1, fill = "blue", color= "black", alpha = 0.6)+ 
  labs(title = " Tenure distribution in the banks", x = "Tenure", y = "Frequency")+
  theme(plot.title= element_text(hjust = 0.5))

# These 2 categories maybe it is good that they are set /1000 (in 1K)
# For the Salary
ggplot(data = cleaned_data, aes(x= Salary))+ 
  geom_histogram(binwidth = 10000, fill = "blue", color= "black", alpha = 0.6)+ 
  labs(title = " Salary distribution in the banks", x = "Salary", y = "Frequency")+
  theme(plot.title= element_text(hjust = 0.5))

# For the Balance
ggplot(data = cleaned_data, aes(x= Balance))+ 
  geom_histogram(binwidth = 10000, fill = "blue", color= "black", alpha = 0.6)+ 
  labs(title = " Balance distribution in the banks", x = "Balance", y = "Frequency")+
  theme(plot.title= element_text(hjust = 0.5))

# Products in use -> why there is half products ???
ggplot(data = cleaned_data, aes(x= Products_in_Use))+ 
  geom_histogram(binwidth = 0.5, fill = "blue", color= "black", alpha = 0.6)+ 
  labs(title = " Products in use distribution in the banks", x = "Products in use", y = "Frequency")+
  theme(plot.title= element_text(hjust = 0.5))

# Exploring the categorical variables with the bar plots: 

# For the gender 
ggplot(data = cleaned_data, aes(x= Gender))+ 
  geom_bar(fill = "blue", alpha = 0.6)+ 
  labs(title = " Gender distribution in the banks", x = "Gender", y = "Frequency")+
  theme(plot.title= element_text(hjust = 0.5))

# For the ones who left or not -> No one left !!! we have to revise that
ggplot(data = cleaned_data, aes(x = factor(Left))) + 
  geom_bar(fill = "blue", alpha = 0.6) + 
  labs(title = "Dropout distribution in the banks", 
       x = "Left or not the bank", 
       y = "Frequency") + 
  theme(plot.title = element_text(hjust = 0.5))


# Before doing the Bivariate analysis, some data transformation will be done to the categorical variables
# One hot encoding techniques will be performed in Gender and Branch:
# It consists of converting the variables to factors and then to numerical. 
# For the Gender:
gender_fact <- factor(cleaned_data$Gender, 
                      levels = c("Female", "Male"))
cleaned_data$Gender <- as.numeric(gender_fact)

# For the Branch: 
branch_fact <- factor(cleaned_data$Branch, 
                      levels = c("Branch1", "Branch2", "Branch3"), 
                      labels = c(0,1,2))
cleaned_data$Branch <- as.numeric(branch_fact)


# BIVARIATE ANALYSIS
# This is done to explore the relationships between 2 variables to find 
# connections, correlations and dependencies

# Filtering the data to exclude the values that are categorical -> taking only the numerical
filtered_data <- cleaned_data %>% select(c(-1,-2,-10))
# The Pearson's and Spearman's Correlation between the variables is applied:
# Pearson correlation: 
# - the variables are continuous and normally distributed, no significant outliers
# - not linear relationships, containing outliers and not normally distributed
correlation_pearson <- cor(filtered_data, method = "pearson")
correlation_spearman <- cor(filtered_data, method = "spearman")

# Plotting the resulting correlation matrix with the Pearson method
corrplot(correlation_pearson, 
         method = "color", 
         type = "upper", 
         tl.cex = 0.8,
         tl.col = "black",
         tl.srt = 45, 
         addCoef.col = "black", 
         title = "Pearson Correlation Heatmap")

# Plotting the resulting correlation matrix with the Spearman method
corrplot(correlation_spearman, 
         method = "color", 
         type = "upper", 
         tl.cex = 0.8,
         tl.col = "black",
         tl.srt = 45, 
         addCoef.col = "black", 
         title = "Spearman Correlation Heatmap")

# So from the heatmap what it can be observed is the following: 
# data doesn't follow a really linear relationships between the features

pairs.panels(filtered_data)


# PCA Analysis
pca_data <- cleaned_data %>% select(c(-9))

scaled_data <- scale(pca_data)

pca_result <- prcomp(scaled_data, center = TRUE, scale.= TRUE)

summary(pca_result)

fviz_eig(pca_result, addlabels = TRUE)

fviz_pca_biplot(pca_result,
                geom.ind = "point", 
                geom.var = "arrow", 
                label = "var", 
                repel = TRUE,
                legend.title = "Left or not")

## WoE and Information values: 

IV <- create_infotables(data = cleaned_data, y = "Left", bins = 10)
# Plotting the WoE for every variable
plot_infotables(IV, "Age")
plot_infotables(IV, "Products_in_Use")
plot_infotables(IV, "Branch")
plot_infotables(IV, "Balance")
plot_infotables(IV, "Gender")
plot_infotables(IV, "Score")
plot_infotables(IV, "Tenure")
plot_infotables(IV, "Salary")

# Multidimensional scaling 

dataset_Dissimatrix <- dist(cleaned_data[1:1000,], method = "manhattan")

datasetMDS <- cmdscale(dataset_Dissimatrix, k = 2)
colnames(datasetMDS)<- c("D1", "D2")
rownames(datasetMDS) <- cleaned_data[1:1000,]$Age
plot(datasetMDS)

# Stress plot
nDimensions <- ncol(cleaned_data)
datasetStress <- vector("numeric", nDimensions)

for (i in 1:nDimensions){
  datasetMDSTest <- cmdscale(dataset_Dissimatrix, k = i)
  datasetMDSDist <- daisy(datasetMDSTest, "gower")
  datasetStress[i] <- sqrt(sum((dataset_Dissimatrix-datasetMDSDist)^2)/sum(dataset_Dissimatrix^2))
}
plot(datasetStress)
