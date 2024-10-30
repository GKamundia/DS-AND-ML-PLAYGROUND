# Load necessary libraries
library(dplyr)
library(ggplot2)
library(readr)
install.packages("zoo")
library(zoo)


# Load the dataset
data_url <- "https://raw.githubusercontent.com/cema-uonbi/L4H_sample_data/main/ideal3a.csv"
ideal_data <- read_csv(data_url)

# Check the structure of the dataset
str(ideal_data)

# Set ReasonsLoss to 0 where ReasonLoss1 indicates survival
ideal_data$ReasonsLoss[ideal_data$ReasonsLoss1 == "survived"] <- 0

# Drop rows with missing values in ELISA_mutans or ELISA_parva
ideal_data <- ideal_data[!is.na(ideal_data$ELISA_mutans) & !is.na(ideal_data$ELISA_parva), ]

# Calculate the mean of ManualPCV for the specific CalfID
mean_manualPCV <- mean(ideal_data$ManualPCV[ideal_data$CalfID == "CA031210334"], na.rm = TRUE)


# Fill missing Weight based on CalfID
ideal_data <- ideal_data %>%
  group_by(CalfID) %>%
  mutate(Weight = ifelse(is.na(Weight), mean(Weight, na.rm = TRUE), Weight)) %>%
  ungroup()

# Fill missing Q.Strongyle.eggs using last observation carried forward (LOCF)
ideal_data <- ideal_data %>%
  arrange(VisitDate) %>%
  mutate(Q.Strongyle.eggs = na.locf(Q.Strongyle.eggs, na.rm = FALSE))

# Fill missing Weight based on CalfID, sublocation, and VisitDate
ideal_data <- ideal_data %>%
  group_by(CalfID) %>%
  mutate(Q.Strongyle.eggs = ifelse(is.na(Q.Strongyle.eggs), mean(Q.Strongyle.eggs, na.rm = TRUE), Q.Strongyle.eggs)) %>%
  ungroup()


# Convert selected variables to factors
ideal_data$ELISA_mutans <- as.factor(ideal_data$ELISA_mutans)
ideal_data$ELISA_parva <- as.factor(ideal_data$ELISA_parva)
ideal_data$Theileria.spp. <- as.factor(ideal_data$Theileria.spp.)
ideal_data$CalfSex <- as.factor(ideal_data$CalfSex)
ideal_data$ReasonsLoss1 <- as.factor(ideal_data$ReasonsLoss1)
ideal_data$ReasonsLoss <- as.factor(ideal_data$ReasonsLoss)
ideal_data$sublocation <- as.factor(ideal_data$sublocation)
ideal_data$Distance_water <- as.factor(ideal_data$Distance_water)

# Check for missing values
summary(ideal_data)


# Conduct logistic regression with ReasonsLoss1 as the target variable
logit_model <- glm(ReasonsLoss1 ~ ELISA_mutans + ELISA_parva + Theileria.spp. + 
                     CalfSex + Distance_water + RecruitWeight + Weight + 
                     ManualPCV + Age + sublocation, 
                   data = ideal_data, family = binomial)

summary(logit_model)

install.packages("broom")
library(broom)
# Step 4: Tidy up the results for easier interpretation
tidy_results <- tidy(logit_model)

# Print all rows of tidy_results
print(tidy_results, n = Inf)

install.packages("pROC")

library(pROC)
# Load necessary package
library(caret)

# ROC Curve and AUC
roc_curve <- roc(ideal_data$ReasonsLoss1, predicted_probabilities)
plot(roc_curve)
auc_value <- auc(roc_curve)
print(auc_value)

# Sensitivity and Specificity
sensitivity <- confusion_matrix$byClass["Sensitivity"]
specificity <- confusion_matrix$byClass["Specificity"]
print(sensitivity)
print(specificity)



