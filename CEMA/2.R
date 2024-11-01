# Load necessary libraries
library(dplyr)
library(ggplot2)
library(readr)
library(zoo)


# Load the dataset
data_url <- "https://raw.githubusercontent.com/cema-uonbi/L4H_sample_data/main/ideal3a.csv"
ideal_data <- read_csv(data_url)

# Check the structure of the dataset
str(ideal_data)

summary(ideal_data)

# Set ReasonsLoss to 0 where ReasonLoss1 indicates survival
ideal_data$ReasonsLoss[ideal_data$ReasonsLoss1 == "survived"] <- 0

# Step 2: Data Preparation
# Convert 'ReasonsLoss1' to binary format (1 = died, 0 = survived)
ideal_data$ReasonsLoss1 <- ifelse(ideal_data$ReasonsLoss1 == "died", 1, 0)

# Step 3: Identify the last visit for each calf
last_visit_data <- ideal_data %>%
  group_by(CalfID) %>%
  filter(VisitWeek == max(VisitWeek)) %>%  # Get the last visit for each calf
  select(CalfID, ReasonsLoss1)              # Select relevant columns

# Step 4: Summarize the results
calf_states <- last_visit_data %>%
  summarise(CalfID, State = ifelse(ReasonsLoss1 == 1, "Died", "Survived"))

# Step 5: View the results
print(calf_states, n = 30)


# Drop rows with missing values in ELISA_mutans or ELISA_parva
#ideal_data <- ideal_data[!is.na(ideal_data$ELISA_mutans) & !is.na(ideal_data$ELISA_parva), ]

# Calculate the mean of ManualPCV for the specific CalfID
#mean_manualPCV <- mean(ideal_data$ManualPCV[ideal_data$CalfID == "CA031210334"], na.rm = TRUE)
#ideal_data$ManualPCV[ideal_data$CalfID == "CA031210334" & is.na(ideal_data$ManualPCV)] <- mean_manualPCV



# Fill missing Weight based on CalfID
#ideal_data <- ideal_data %>%
#  group_by(CalfID) %>%
#  mutate(Weight = ifelse(is.na(Weight), mean(Weight, na.rm = TRUE), Weight)) %>%
#  ungroup()

# Fill missing Q.Strongyle.eggs using last observation carried forward (LOCF)
#ideal_data <- ideal_data %>%
#  arrange(VisitDate) %>%
#  mutate(Q.Strongyle.eggs = na.locf(Q.Strongyle.eggs, na.rm = FALSE))

# Fill missing Weight based on CalfID, sublocation, and VisitDate
#ideal_data <- ideal_data %>%
#  group_by(CalfID) %>%
#  mutate(Q.Strongyle.eggs = ifelse(is.na(Q.Strongyle.eggs), mean(Q.Strongyle.eggs, na.rm = TRUE), Q.Strongyle.eggs)) %>%
#  ungroup()


# Convert selected variables to factors
ideal_data$ELISA_mutans <- as.factor(ideal_data$ELISA_mutans)
ideal_data$ELISA_parva <- as.factor(ideal_data$ELISA_parva)
ideal_data$Theileria.spp. <- as.factor(ideal_data$Theileria.spp.)
ideal_data$CalfSex <- as.factor(ideal_data$CalfSex)
ideal_data$ReasonsLoss1 <- as.factor(ideal_data$ReasonsLoss1)
ideal_data$ReasonsLoss <- as.factor(ideal_data$ReasonsLoss)
ideal_data$sublocation <- as.factor(ideal_data$sublocation)
ideal_data$Distance_water <- as.factor(ideal_data$Distance_water)
ideal_data$Education <- as.factor(ideal_data$Education)

# Here, we simply drop rows with NA values for simplicity
ideal_data <- na.omit(ideal_data)

# Check for missing values
summary(ideal_data)


# Split the dataset into training and testing sets (80/20 split)
set.seed(123)  # Set seed for reproducibility
train_index <- createDataPartition(ideal_data$ReasonsLoss1, p = 0.8, list = FALSE)
train_data <- ideal_data[train_index, ]
test_data <- ideal_data[-train_index, ]

# Conduct logistic regression with ReasonsLoss1 as the target variable
logit_model1 <- glm(ReasonsLoss1 ~ ELISA_mutans + ELISA_parva +  Theileria.spp. + 
                      CalfSex + Distance_water+ Education  + sublocation, 
                    data = train_data, family = binomial)

# Model summary
summary(logit_model1)
# Step 4: Check distribution of the target variable (survived vs died)
# Filter the dataset to keep only the last visit for each calf
last_visit_data <- ideal_data %>%
  group_by(CalfID) %>%
  filter(VisitDate == max(VisitDate)) %>%
  ungroup()

# Create the bar plot for ReasonsLoss1 at the last visit
ggplot(last_visit_data, aes(x = as.factor(ReasonsLoss1))) + 
  geom_bar(aes(y = ..count..), fill = "skyblue") + 
  labs(x = "Reasons Loss (0 = Survived, 1 = Died)", y = "Count") +
  theme_minimal() +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5)  # Add counts above bars
# Make predictions on the test set
test_data$predicted_prob <- predict(logit_model1, newdata = test_data, type = "response")
test_data$predicted_class <- ifelse(test_data$predicted_prob > 0.5, "died", "survived")

# Confusion matrix
conf_matrix <- confusionMatrix(as.factor(test_data$predicted_class), as.factor(test_data$ReasonsLoss1))
print(conf_matrix)

# Calculate Precision, Recall, and F1 Score
precision <- posPredValue(test_data$predicted_class, test_data$ReasonsLoss1, positive = "died")
recall <- sensitivity(test_data$predicted_class, test_data$ReasonsLoss1, positive = "died")
f1_score <- (2 * precision * recall) / (precision + recall)

# Output Precision, Recall, F1 Score
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")

# Area Under the ROC Curve (AUC-ROC)
roc_curve <- roc(test_data$ReasonsLoss1, test_data$predicted_prob)
auc_value <- auc(roc_curve)

# Print AUC value
cat("AUC-ROC:", auc_value, "\n")

# Variable contribution analysis
tidy_results <- tidy(logit_model)

# Print all rows of tidy_results
print(tidy_results, n = Inf)



