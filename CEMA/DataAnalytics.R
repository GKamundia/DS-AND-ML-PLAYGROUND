install.packages("lm34")
install.packages("tidyverse")

#Loading necessary libraries
library(tidyverse)
library(lme4)
library(broom)
library(dplyr)
library(readr)
library(car)
library(ggplot2)
library(caret)

# Step 1: Load the dataset
data_url <- "https://raw.githubusercontent.com/cema-uonbi/L4H_sample_data/main/ideal3a.csv"
ideal_data <- read_csv(data_url)

# Step 2: Data Preparation
# Set ReasonsLoss to 0 where ReasonLoss1 indicates survival
ideal_data$ReasonsLoss[ideal_data$ReasonsLoss1 == "survived"] <- 0
# Convert 'ReasonsLoss1' to binary (1 = died, 0 = survived)
ideal_data$ReasonsLoss1 <- ifelse(ideal_data$ReasonsLoss1 == "died", 1, 0)

# Check the unique values of ReasonsLoss1
print(unique(ideal_data$ReasonsLoss1))

# Check counts of each category
print(table(ideal_data$ReasonsLoss1))

# Convert categorical variables to factors
ideal_data$CalfSex <- as.factor(ideal_data$CalfSex)
ideal_data$Education <- as.factor(ideal_data$Education)
ideal_data$Distance_water <- as.factor(ideal_data$Distance_water)

# Handle missing values (e.g., imputation or removal)
# Here, we simply drop rows with NA values for simplicity
ideal_data <- na.omit(ideal_data)


# Visualize the distribution of the target variable
ggplot(ideal_data, aes(x = as.factor(ReasonsLoss1))) + 
  geom_bar() + 
  labs(x = "Reasons Loss (0 = Survived, 1 = Died)", y = "Count") +
  theme_minimal()

# Fit a preliminary model to check multicollinearity
vif_model <- glm(ReasonsLoss1 ~ CalfSex + Education + Distance_water + RecruitWeight + 
                   Age + ManualPCV + Theileria.spp. + ELISA_mutans + ELISA_parva + 
                   Q.Strongyle.eggs, 
                 family = binomial(link = "logit"), 
                 data = ideal_data)

# Check VIF values
vif_values <- vif(vif_model)
print(vif_values)


library(smotefamily)

# Apply SMOTE to handle class imbalance
set.seed(123)  # Set seed for reproducibility
# The smote function uses a slightly different syntax
smote_data <- SMOTE(ReasonsLoss1 ~ CalfSex + Education + Distance_water + RecruitWeight + 
                      Age + ManualPCV + Theileria.spp. + ELISA_mutans + ELISA_parva + 
                      Q.Strongyle.eggs, 
                    data = ideal_data, perc.over = 100, k = 5)  # `k` is the number of nearest neighbors

# Check the distribution after SMOTE
print(table(smote_data$ReasonsLoss1))




# Check the distribution after applying SMOTE
print(table(balanced_data$ReasonsLoss1))

# Split the data into training and testing sets (80% training, 20% testing)
train_index <- createDataPartition(ideal_data$ReasonsLoss1, p = 0.8, list = FALSE)
train_data <- ideal_data[train_index, ]
test_data <- ideal_data[-train_index, ]


# Fit the logistic regression model
model <- glm(ReasonsLoss1 ~ CalfSex + Education + Distance_water + RecruitWeight + 
               Age + ManualPCV + Theileria.spp. + ELISA_mutans + ELISA_parva + 
               Q.Strongyle.eggs, 
             family = binomial(link = "logit"), 
             data = train_data)

# Summary of the model
summary(model)

# Make predictions on the test data
test_data$predicted_prob <- predict(model, newdata = test_data, type = "response")

# Classify predictions based on a threshold (commonly 0.5)
test_data$predicted_class <- ifelse(test_data$predicted_prob > 0.5, 1, 0)


# Load necessary library for confusion matrix
library(caret)

# Confusion matrix
confusion_matrix <- confusionMatrix(as.factor(test_data$predicted_class), as.factor(test_data$ReasonsLoss1))
print(confusion_matrix)

# Additional performance metrics
accuracy <- sum(test_data$predicted_class == test_data$ReasonsLoss1) / nrow(test_data)
cat("Accuracy:", accuracy, "\n")


