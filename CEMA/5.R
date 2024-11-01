# Load necessary libraries
library(tidyverse)
library(lme4)  # For mixed-effects modeling
library(broom.mixed)  # For tidying model output

# Step 1: Load the dataset
ideal_data <- read.csv("C:/Users/Anarchy/Documents/Data_Science/CEMA/L4H_sample_data-main/ideal3a.csv")

# Set ReasonsLoss to 0 where ReasonLoss1 indicates survival
ideal_data$ReasonsLoss[ideal_data$ReasonsLoss1 == "survived"] <- 0
# Convert 'ReasonsLoss1' to binary (1 = died, 0 = survived)
ideal_data$ReasonsLoss1 <- ifelse(ideal_data$ReasonsLoss1 == "died", 1, 0)

# Fill missing Weight based on CalfID, sublocation, and VisitDate NOTE: Ac
ideal_data <- ideal_data %>%
  group_by(CalfID) %>%
  mutate(Q.Strongyle.eggs = ifelse(is.na(Q.Strongyle.eggs), mean(Q.Strongyle.eggs, na.rm = TRUE), Q.Strongyle.eggs)) %>%
  ungroup()

# Fill missing Weight based on CalfID, sublocation, and VisitDate NOTE: Acc= 0.25, AUC= 0.75
ideal_data <- ideal_data %>%
  group_by(CalfID) %>%
  mutate(Q.Strongyle.eggs = ifelse(is.na(Q.Strongyle.eggs), mean(Q.Strongyle.eggs, na.rm = TRUE), Q.Strongyle.eggs)) %>%
  ungroup()

num_vars <- c("Weight", "Age", "ManualPCV", "RecruitWeight", "Q.Strongyle.eggs")


ideal_data[num_vars] <- scale(ideal_data[num_vars])
# Step 2: Data Preparation
ideal_data <- ideal_data %>% na.omit()

# Convert categorical variables to factors
ideal_data <- ideal_data %>%
  mutate(
    CalfID = as.factor(CalfID),  # Ensure to include CalfID for random effects
    CalfSex = as.factor(CalfSex),
    Education = as.factor(Education),
    Distance_water = as.factor(Distance_water),
    ELISA_mutans = as.factor(ELISA_mutans),
    ELISA_parva = as.factor(ELISA_parva),
    Theileria.spp. = as.factor(Theileria.spp.),
    sublocation = as.factor(sublocation)
  )

# Step 3: Fit the Mixed-Effects Model
mixed_model <- lmer(ADWG ~ CalfSex + Education + Distance_water + Weight + Age + 
                      ManualPCV + Theileria.spp. + ELISA_parva + 
                      Q.Strongyle.eggs + (1 | CalfID), data = ideal_data)


# Step 4: Model summary
mixed_model_summary <- summary(mixed_model)
print(mixed_model_summary)


# Step 5: Model Diagnostics
# Residuals vs Fitted
plot(mixed_model, which = 1)



# Histogram of residuals
ggplot(data = as.data.frame(residuals(mixed_model)), aes(x = residuals(mixed_model))) +
  geom_histogram(bins = 30) +
  labs(title = "Residuals Distribution", x = "Residuals", y = "Frequency") +
  theme_minimal()


# Step 6: Interpretation
 Coefficients of fixed effects
fixed_effects <- broom.mixed::tidy(mixed_model)


# Variance components for random effects
random_effects <- VarCorr(mixed_model)
print(random_effects)

