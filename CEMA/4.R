# Step 1: Load necessary libraries
library(tidyverse)  # For data manipulation and visualization
library(lubridate)  # For date handling
library(broom)      # For tidy model outputs
library(caret)      # For creating confusion matrices

# Step 2: Load the dataset
data_url <- "https://raw.githubusercontent.com/cema-uonbi/L4H_sample_data/main/ideal3a.csv"
ideal_data <- read_csv(data_url)

# Display the structure of the dataset
str(ideal_data)

# Step 3: Data Cleaning and Preparation
# Step 3.1: Set ReasonsLoss to 0 where ReasonsLoss1 indicates survival
ideal_data$ReasonsLoss[ideal_data$ReasonsLoss1 == "survived"] <- 0
# Convert 'ReasonsLoss1' to binary (1 = died, 0 = survived)
ideal_data$ReasonsLoss1 <- ifelse(ideal_data$ReasonsLoss1 == "died", 1, 0)

# Step 3.2: Convert 'VisitDate' to Date format
ideal_data$VisitDate <- as.Date(ideal_data$VisitDate)

# Step 3.3: Drop rows with NA values
ideal_data <- na.omit(ideal_data)

# Step 3.4: Filter for only 30 unique calves
unique_calves <- unique(ideal_data$CalfID)  # Get unique calf IDs
if(length(unique_calves) > 30) {
  # If more than 30 unique calves, filter for the first 30
  unique_calves <- unique_calves[1:30]
}
ideal_data <- ideal_data %>% filter(CalfID %in% unique_calves)

# Step 4: Create a Unique Identifier and Reshape Data
ideal_data <- ideal_data %>%
  arrange(CalfID, VisitDate) %>%  # Sort data by CalfID and VisitDate
  group_by(CalfID) %>%
  mutate(VisitID = row_number()) %>%  # Create a VisitID for each row
  ungroup()

# Step 5: Create changes in health indicators
ideal_data <- ideal_data %>%
  group_by(CalfID) %>%
  arrange(VisitDate) %>%
  mutate(WeightChange = Weight - lag(Weight, default = first(Weight)),  # Change in weight
         PCVChange = ManualPCV - lag(ManualPCV, default = first(ManualPCV)),  # Change in Manual PCV
         QStrongyleChange = Q.Strongyle.eggs - lag(Q.Strongyle.eggs, default = first(Q.Strongyle.eggs))) %>%  # Change in Strongyle eggs
  ungroup()

# Step 6: Create Time-Related Features
ideal_data <- ideal_data %>%
  mutate(VisitNumber = row_number(),  # Sequence number of the visit
         TimeSinceFirstVisit = as.numeric(VisitDate - min(VisitDate)))  # Days since the first visit

# Step 7: Handling Categorical Variables
ideal_data$CalfSex <- as.factor(ideal_data$CalfSex)
ideal_data$Education <- as.factor(ideal_data$Education)
ideal_data$Distance_water <- as.factor(ideal_data$Distance_water)

# Step 8: Split the data into training and testing sets (80% training, 20% testing)
set.seed(123)  # For reproducibility
train_index <- createDataPartition(ideal_data$ReasonsLoss1, p = 0.8, list = FALSE)
train_data <- ideal_data[train_index, ]
test_data <- ideal_data[-train_index, ]

# Step 9: Fit the logistic regression model
model <- glm(ReasonsLoss1 ~ CalfSex + Education + Distance_water + VisitNumber + TimeSinceFirstVisit + 
               RecruitWeight + Age + ManualPCV + Theileria.spp. + ELISA_mutans + ELISA_parva + 
               Q.Strongyle.eggs, 
             family = binomial(link = "logit"), 
             data = train_data)

# Step 10: Summary of the model
summary(model)

# Step 11: Make predictions on the test data
test_data$predicted_prob <- predict(model, newdata = test_data, type = "response")

# Step 12: Classify predictions based on a threshold (commonly 0.5)
test_data$predicted_class <- ifelse(test_data$predicted_prob > 0.5, 1, 0)

# Step 13: Confusion matrix
confusion_matrix <- confusionMatrix(as.factor(test_data$predicted_class), as.factor(test_data$ReasonsLoss1))
print(confusion_matrix)

# Step 14: Display predictions and their associated factors
predictions <- test_data %>%
  select(CalfID, VisitDate, ReasonsLoss1, predicted_prob, predicted_class, Weight, ManualPCV, Q.Strongyle.eggs)
print(predictions, n = 22)

