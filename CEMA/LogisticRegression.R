#Loading necessary libraries
library(tidyverse)
library(lme4)
library(broom)
library(dplyr)
library(readr)
library(car)
library(ggplot2)
library(caret)
library(vip)
library(pROC)


# Step 1: Load the dataset
#data_url <- "https://raw.githubusercontent.com/cema-uonbi/L4H_sample_data/main/ideal3a.csv"
#ideal_data <- read_csv(data_url)
ideal_data <- read.csv("C:/Users/Anarchy/Documents/Data_Science/CEMA/L4H_sample_data-main/ideal3a.csv")

summary(ideal_data)
#view(ideal_data)
# This plot shows which columns have missing values together
gg_miss_upset(ideal_data)


# Step 2: Data Preparation
# Set ReasonsLoss to 0 where ReasonLoss1 indicates survival
ideal_data$ReasonsLoss[ideal_data$ReasonsLoss1 == "survived"] <- 0
# Convert 'ReasonsLoss1' to binary (1 = died, 0 = survived)
ideal_data$ReasonsLoss1 <- ifelse(ideal_data$ReasonsLoss1 == "died", 1, 0)

# Calculate the mean of ManualPCV for the specific CalfID
mean_manualPCV <- mean(ideal_data$ManualPCV[ideal_data$CalfID == "CA031210334"], na.rm = TRUE)
ideal_data$ManualPCV[ideal_data$CalfID == "CA031210334" & is.na(ideal_data$ManualPCV)] <- mean_manualPCV



# Fill missing Weight based on CalfID
#ideal_data <- ideal_data %>%
#  group_by(CalfID) %>%
#  mutate(Weight = ifelse(is.na(Weight), mean(Weight, na.rm = TRUE), Weight)) %>%
#  ungroup()

# Fill missing Q.Strongyle.eggs using last observation carried forward (LOCF)
#ideal_data <- ideal_data %>%
#  arrange(VisitDate) %>%
#  mutate(Q.Strongyle.eggs = na.locf(Q.Strongyle.eggs, na.rm = FALSE))

# Fill missing Weight based on CalfID, sublocation, and VisitDate NOTE: Acc= 0.25, AUC= 0.75
ideal_data <- ideal_data %>%
  group_by(CalfID) %>%
  mutate(Q.Strongyle.eggs = ifelse(is.na(Q.Strongyle.eggs), mean(Q.Strongyle.eggs, na.rm = TRUE), Q.Strongyle.eggs)) %>%
  ungroup()

summary(ideal_data)

# Convert categorical variables to factors
ideal_data <- ideal_data %>%
  mutate(
    CalfSex = as.factor(CalfSex),
    Education = as.factor(Education),
    Distance_water = as.factor(Distance_water),
    ELISA_mutans = as.factor(ELISA_mutans),
    ELISA_parva = as.factor(ELISA_parva),
    Theileria.spp. = as.factor(Theileria.spp.),
    ReasonsLoss = as.factor(ReasonsLoss),
    sublocation = as.factor(sublocation)
  )



# This keeps only the last record for each CalfID based on VisitDate
latest_data <- ideal_data %>%
  group_by(CalfID) %>%
  filter(VisitDate == max(VisitDate)) %>%
  ungroup()

summary(latest_data)

# Here, we simply drop rows with NA values for simplicity
latest_data <- na.omit(latest_data)

summary(latest_data)

num_vars <- c("Weight", "Age", "ManualPCV", "RecruitWeight", "Q.Strongyle.eggs")


latest_data[num_vars] <- scale(latest_data[num_vars])

#view(latest_data)

# Step 5: Check distribution of the target variable (survived vs died) with counts
ggplot(latest_data, aes(x = as.factor(ReasonsLoss1))) + 
  geom_bar() + 
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +  # Add count labels
  labs(x = "Reasons Loss (0 = Survived, 1 = Died)", y = "Count") +
  theme_minimal()

# Step 6: Check multicollinearity using VIF
vif_model <- glm(ReasonsLoss1 ~ CalfSex + Education + Distance_water + RecruitWeight + 
                   Age + ManualPCV + Theileria.spp. + ELISA_mutans + ELISA_parva + 
                   Q.Strongyle.eggs, 
                 family = binomial(link = "logit"), 
                 data = latest_data)

vif_values <- vif(vif_model)
print(vif_values)

# Step 7: Split the data into training and testing sets (70% training, 30% testing)
set.seed(123)  # For reproducibility
train_index <- createDataPartition(latest_data$ReasonsLoss1, p = 0.7, list = FALSE)
train_data <- latest_data[train_index, ]
test_data <- latest_data[-train_index, ]

# Identify any factors with a single level in train_data
single_level_factors <- sapply(train_data, function(x) is.factor(x) && length(unique(x)) < 2)
print(single_level_factors)


# Step 8: Fit a standard logistic regression model
logistic_model <- glm(ReasonsLoss1 ~ CalfSex + Education +  RecruitWeight + Weight + 
                        Age + ManualPCV + Theileria.spp. + ELISA_parva + 
                        Q.Strongyle.eggs, 
                      family = binomial(link = "logit"), 
                      data = train_data)

# Step 9: Model summary
summary(logistic_model)

# Step 10: Make predictions on the test set
test_data$predicted_prob <- predict(logistic_model, newdata = test_data, type = "response")
test_data$predicted_class <- ifelse(test_data$predicted_prob > 0.5, 1, 0)  # Use 1 for "died"

# Step 11: Confusion matrix
conf_matrix <- confusionMatrix(as.factor(test_data$predicted_class), as.factor(test_data$ReasonsLoss1))
print(conf_matrix)


# Extracting values from confusion matrix
TP <- conf_matrix$table[2, 2]  # True Positives
TN <- conf_matrix$table[1, 1]  # True Negatives
FP <- conf_matrix$table[1, 2]  # False Positives
FN <- conf_matrix$table[2, 1]  # False Negatives

# Calculate precision, recall, and F1 score
precision <- TP / (TP + FP)      # Positive Predictive Value
recall <- TP / (TP + FN)          # Sensitivity
F1_score <- 2 * (precision * recall) / (precision + recall)

# Output the F1 score
cat("F1 Score:", F1_score, "\n")

# Variable Importance
importance <- varImp(logistic_model, scale = FALSE)
print(importance, row.names = TRUE)

# Visualization of variable importance
vip(logistic_model)

# Step 1: Create the ROC curve
roc_curve <- roc(test_data$ReasonsLoss1, test_data$predicted_prob)

# Step 2: Plot the ROC curve# Optionally: Add AUC to the plot
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)

# Step 3: Calculate and print the AUC
auc_value <- auc(roc_curve)
cat("AUC:", auc_value, "\n")





