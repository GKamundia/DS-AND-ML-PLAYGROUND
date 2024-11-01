# Loading necessary libraries
library(tidyverse)
library(car)
library(caret)

# Load the dataset
data_url <- "https://raw.githubusercontent.com/cema-uonbi/L4H_sample_data/main/ideal3a.csv"
ideal_data <- read_csv(data_url)

# Set ReasonsLoss to 0 where ReasonLoss1 indicates survival
ideal_data$ReasonsLoss[ideal_data$ReasonsLoss1 == "survived"] <- 0

# Step 2: Data Preparation
# Convert 'ReasonsLoss1' to binary format (1 = died, 0 = survived)
ideal_data$ReasonsLoss1 <- ifelse(ideal_data$ReasonsLoss1 == "died", 1, 0)

# Step 1: Data Preparation
# Aggregate data by the last recorded entry for each calf (or use averages if suitable)
ideal_data_agg <- ideal_data %>%
  group_by(CalfID) %>%
  summarize(
    ReasonsLoss1 = last(ReasonsLoss1),
    CalfSex = first(CalfSex),
    Age = mean(Age, na.rm = TRUE),
    RecruitWeight = mean(RecruitWeight, na.rm = TRUE),
    Distance_water = first(Distance_water),
    Theileria_spp = mean(Theileria.spp., na.rm = TRUE),
    ELISA_mutans = mean(ELISA_mutans, na.rm = TRUE),
    ELISA_parva = mean(ELISA_parva, na.rm = TRUE),
    Q_Strongyle_eggs = mean(Q.Strongyle.eggs, na.rm = TRUE)
  )

# Step 2: Split data into training and testing sets
set.seed(123)  # For reproducibility
train_index <- createDataPartition(ideal_data_agg$ReasonsLoss1, p = 0.8, list = FALSE)
train_data <- ideal_data_agg[train_index, ]
test_data <- ideal_data_agg[-train_index, ]

# Step 3: Fit logistic regression model on the training data
model <- glm(ReasonsLoss1 ~ CalfSex + Age + RecruitWeight + Distance_water +
               Theileria_spp + ELISA_mutans + ELISA_parva + Q_Strongyle_eggs,
             family = binomial(link = "logit"),
             data = train_data)

# Step 4: Make predictions on the test data
# Predict probabilities
test_predictions_prob <- predict(model, test_data, type = "response")

# Convert probabilities to binary outcomes (0 for survival, 1 for death)
test_predictions <- ifelse(test_predictions_prob > 0.5, 1, 0)

# Step 5: Confusion Matrix
# Note: Ensure `ReasonsLoss1` is binary (0 for survival, 1 for death) in test_data
conf_matrix <- confusionMatrix(
  factor(test_predictions), 
  factor(test_data$ReasonsLoss1),
  positive = "1"
)

# Print confusion matrix and overall model accuracy
print(conf_matrix)
