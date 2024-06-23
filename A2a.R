#Setting working directory
setwd("E:\\VCU\\Summer 2024\\Statistical Analysis & Modeling")

#Install packages
install.packages("car")

# Load necessary libraries
library(tidyr)
library(ggplot2)
library(car)
library(stats)
library(readr)

# Load the dataset
data <- read_csv("NSSO68.csv")
unique(data$state_1)

#Subset data to state assigned
subset_data <- data%>%
  filter(state_1 == 'KE') %>%
  select(foodtotal_q, MPCE_MRP, MPCE_URP, Age, Possess_ration_card, Education, Meals_At_Home, No_of_Meals_per_day)
print(subset_data)

# Check for missing values
sum(is.na(subset_data$MPCE_MRP))
sum(is.na(subset_data$MPCE_URP))
sum(is.na(subset_data$Age))
sum(is.na(subset_data$Possess_ration_card))
sum(is.na(data$Education))
sum(is.na(subset_data$Meals_At_Home))
sum(is.na(subset_data$No_of_Meals_per_day))
sum(is.na(subset_data$foodtotal_q))

# Replace missing values with mean
impute_with_mean <- function(data, columns) {
  data %>%
    mutate(across(all_of(columns), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
}
# Verify that there are no more missing values
missing_values_after <- sapply(subset_data, function(x) sum(is.na(x)))
cat("Missing values after replacement:\n")
print(missing_values_after)

# Fit the regression model
model <- lm(foodtotal_q~ MPCE_MRP+MPCE_URP+Age+Meals_At_Home+Possess_ration_card+Education, data = subset_data)

# Print the regression results
print(summary(model))

# Check for multicollinearity using Variance Inflation Factor (VIF)
vif(model) # VIF Value more than 8 its problematic

# Extract the coefficients from the model
coefficients <- coef(model)

# Construct the equation
equation <- paste0("y = ", round(coefficients[1], 2))
for (i in 2:length(coefficients)) {
  equation <- paste0(equation, " + ", round(coefficients[i], 6), "*x", i-1)
}

# Print the equation
print(equation)

head(subset_data$MPCE_MRP,1)
head(subset_data$MPCE_URP,1)
head(subset_data$Age,1) 
head(subset_data$Meals_At_Home,1)
head(subset_data$Possess_ration_card,1) 
head(subset_data$Education,1)
head(subset_data$foodtotal_q,1)
