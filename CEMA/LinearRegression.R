# Load necessary libraries
library(lme4)
library(dplyr)
library(ggplot2)
library(car)
library(broom.mixed)
library(MuMIn)


# Step 1: Load the dataset
ideal_data <- read.csv("C:/Users/Anarchy/Documents/Data_Science/CEMA/L4H_sample_data-main/ideal3a.csv")

# Set ReasonsLoss to 0 where ReasonLoss1 indicates survival
ideal_data$ReasonsLoss[ideal_data$ReasonsLoss1 == "survived"] <- 0

# Convert 'ReasonsLoss1' to binary (1 = died, 0 = survived)
ideal_data$ReasonsLoss1 <- ifelse(ideal_data$ReasonsLoss1 == "died", 1, 0)

# Calculate the mean of ManualPCV for the specific CalfID
mean_manualPCV <- mean(ideal_data$ManualPCV[ideal_data$CalfID == "CA031210334"], na.rm = TRUE)
ideal_data$ManualPCV[ideal_data$CalfID == "CA031210334" & is.na(ideal_data$ManualPCV)] <- mean_manualPCV


# Fill missing Q.Strongyle.eggs based on CalfID group
ideal_data <- ideal_data %>%
  group_by(CalfID) %>%
  mutate(Q.Strongyle.eggs = ifelse(is.na(Q.Strongyle.eggs), mean(Q.Strongyle.eggs, na.rm = TRUE), Q.Strongyle.eggs)) %>%
  ungroup()

# Scale numerical variables
#num_vars <- c("Weight", "Age", "ManualPCV", "RecruitWeight", "Q.Strongyle.eggs")
#ideal_data[num_vars] <- scale(ideal_data[num_vars])

# Remove any rows with missing data
ideal_data <- ideal_data %>% na.omit()


# Convert categorical variables to factors
ideal_data <- ideal_data %>%
  mutate(
    CalfID = as.factor(CalfID),
    CalfSex = as.factor(CalfSex),
    Education = as.factor(Education),
    Distance_water = as.factor(Distance_water),
    ELISA_mutans = as.factor(ELISA_mutans),
    ELISA_parva = as.factor(ELISA_parva),
    Theileria.spp. = as.factor(Theileria.spp.),
    sublocation = as.factor(sublocation)
  )

summary(ideal_data)


# EDA: Visualize distribution of numerical variables
num_vars <- c("ADWG", "Weight", "ManualPCV", "Q.Strongyle.eggs", "RecruitWeight")
par(mfrow = c(2, 2))
for (var in num_vars) {
  ggplot(ideal_data, aes_string(x = var)) +
    geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
    labs(title = paste("Distribution of", var), x = var, y = "Frequency") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}

# EDA: Check for missing values
missing_values <- colSums(is.na(ideal_data))
print(missing_values)

# EDA: Visualize relationships between variables
ggplot(ideal_data, aes(x = Weight, y = ADWG, color = CalfSex)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  labs(title = "Relationship between Weight and ADWG by Calf Sex", x = "Weight", y = "ADWG") +
  theme_minimal()

# EDA: Boxplot for categorical variable analysis
ggplot(ideal_data, aes(x = CalfSex, y = ADWG)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "ADWG by Calf Sex", x = "Calf Sex", y = "ADWG") +
  theme_minimal()

# Step 2: Fit the Mixed-Effects Model
mixed_model <- lmer(ADWG ~ CalfSex + Education + Distance_water +  RecruitWeight +
                      ManualPCV + Theileria.spp. + ELISA_parva + 
                      Q.Strongyle.eggs + (1 | CalfID), 
                    data = ideal_data, 
                    control = lmerControl(optimizer = "bobyqa")
                    )


# Step 3: Model Summary
mixed_model_summary <- summary(mixed_model)
print(mixed_model_summary)

# Step 4: Model Diagnostics
# Residuals vs Fitted plot
par(mfrow = c(2, 2))
plot(mixed_model)

# Check for multicollinearity using VIF
vif_values <- vif(mixed_model)
print(vif_values)

# Step 5: Predictions and Evaluation
# Store residuals in the data frame
ideal_data$residuals <- residuals(mixed_model)

# Calculate RMSE
rmse <- sqrt(mean(ideal_data$residuals^2))
cat("RMSE:", rmse, "\n")

# Calculate R-squared for mixed-effects models
r_squared <- r.squaredGLMM(mixed_model)
cat("R-squared (marginal):", r_squared[1], "\n")
cat("R-squared (conditional):", r_squared[2], "\n")

