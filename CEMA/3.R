# Load necessary libraries
library(tidyverse)  # For data manipulation and visualization
library(lubridate)  # For date handling
library(broom)      # For tidy model outputs

# Load the dataset
data_url <- "https://raw.githubusercontent.com/cema-uonbi/L4H_sample_data/main/ideal3a.csv"
ideal_data <- read_csv(data_url)

# Set ReasonsLoss to 0 where ReasonLoss1 indicates survival
ideal_data$ReasonsLoss[ideal_data$ReasonsLoss1 == "survived"] <- 0
# Convert 'ReasonsLoss1' to binary (1 = died, 0 = survived)
ideal_data$ReasonsLoss1 <- ifelse(ideal_data$ReasonsLoss1 == "died", 1, 0)

# Step 3: Data Cleaning and Preparation
# Convert 'VisitDate' to Date format (if not already in Date format)
ideal_data$VisitDate <- as.Date(ideal_data$VisitDate)

# Here, we simply drop rows with NA values for simplicity
ideal_data <- na.omit(ideal_data)

# Step 4: Create a Unique Identifier and Reshape Data
ideal_data <- ideal_data %>%
  arrange(CalfID, VisitDate) %>%  # Sort data by CalfID and VisitDate
  group_by(CalfID) %>%
  mutate(VisitID = row_number()) %>%  # Create a VisitID for each row
  ungroup()

# Create changes in health indicators
ideal_data <- ideal_data %>%
  group_by(CalfID) %>%
  arrange(VisitDate) %>%
  mutate(WeightChange = Weight - lag(Weight, default = first(Weight)),  # Change in weight
         PCVChange = ManualPCV - lag(ManualPCV, default = first(ManualPCV)),  # Change in Manual PCV
         QStrongyleChange = Q.Strongyle.eggs - lag(Q.Strongyle.eggs, default = first(Q.Strongyle.eggs))) %>%  # Change in Strongyle eggs
  ungroup()

# Step 5: Create Time-Related Features
ideal_data <- ideal_data %>%
  mutate(VisitNumber = row_number(),  # Sequence number of the visit
         TimeSinceFirstVisit = as.numeric(VisitDate - min(VisitDate)))  # Days since the first visit

# Step 6: Handling Categorical Variables
ideal_data$CalfSex <- as.factor(ideal_data$CalfSex)
ideal_data$Education <- as.factor(ideal_data$Education)
ideal_data$Distance_water <- as.factor(ideal_data$Distance_water)

# Check for missing values
summary(ideal_data)

view(ideal_data)

# Split the data into training and testing sets (80% training, 20% testing)
train_index <- createDataPartition(ideal_data$ReasonsLoss1, p = 0.8, list = FALSE)
train_data <- ideal_data[train_index, ]
test_data <- ideal_data[-train_index, ]


# Fit the logistic regression model
model <- glm(ReasonsLoss1 ~ CalfSex + Education + Distance_water + VisitNumber + TimeSinceFirstVisit + RecruitWeight + 
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
