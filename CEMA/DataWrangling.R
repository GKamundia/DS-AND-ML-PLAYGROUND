setwd("C:/Users/Anarchy/Documents/Data_Science/CEMA/L4H_sample_data-main")
install.packages(c("dplyr", "janitor", "lubridate"))
install.packages("readr")

library(readr)
library(dplyr)
library(janitor)
library(lubridate)

list.files("sample_data")

# Import the data using the exact file names
baseline_household <- read_csv("sample_data/L4H_household_baseline_sample.csv")
baseline_individual <- read_csv("sample_data/L4H_individual_baseline_sample.csv")
baseline_mother <- read_csv("sample_data/L4H_mother_baseline_sample.csv")

baseline_household <- clean_names(baseline_household)
baseline_individual <- clean_names(baseline_individual)
baseline_mother <- clean_names(baseline_mother)

# Filter rows where hh_eligible is 1
baseline_household_filtered <- baseline_household %>% 
  filter(hh_eligible == 1)

#Merging baseline_individual data and baseline mother data 
merged_data <- baseline_individual %>% 
  inner_join(baseline_mother, by = c("number" = "number_0"))

# Final merge
final_data <- merged_data %>% 
  inner_join(baseline_household_filtered, by = "household_id")

View(final_data)

str(final_data)

# Check the column names in final_data
colnames(final_data)

# Select the columns of interest
columns_of_interest <- c("reason_for_ineligibility", "rspntgndr", "rspndtmarital", "rspndt_eductn", "maincme")

# Display the first 10 entries for these columns
head(final_data[columns_of_interest], 20)

# Couldn't find column for H_hfrml_eductn

unique(final_data$reason_for_ineligibility)
unique(final_data$rspntgndr)
unique(final_data$h_hfrml_eductn)
unique(final_data$rspndtmarital)
unique(final_data$rspndt_eductn)
unique(final_data$maincme)


# Recode the variables using the mappings from the data dictionary
final_data <- final_data %>%
  mutate(
    reason_for_ineligibility = recode(reason_for_ineligibility,
                                    `1` = "No adult occupier >16 years",
                                    `2` = "Withdrawal",
                                    `3` = "Other reason"),
    
    rspntgndr = recode(rspntgndr,
                       `1` = "Male",
                       `2` = "Female"),
    
    h_hfrml_eductn = recode(h_hfrml_eductn,
                            `1` = "No formal education",
                            `2` = "Primary School",
                            `3` = "Secondary school",
                            `4` = "College-graduate",
                            `5` = "Madrassa",
                            `6` = "Other"),
    
    rspndtmarital = recode(rspndtmarital,
                           `1` = "Single",
                           `2` = "Married monogamous",
                           `3` = "Married polygamous",
                           `4` = "Divorced/separated",
                           `5` = "Widow(er)"),
    
    rspndt_eductn = recode(rspndt_eductn,
                           `1` = "No formal education",
                           `2` = "Primary School",
                           `3` = "Secondary school",
                           `4` = "College-graduate",
                           `5` = "Madrassa",
                           `6` = "Other"),
    
    maincme = recode(maincme,
                     `1` = "Sale of livestock & livestock products",
                     `2` = "Sale of crops",
                     `3` = "Trading/business",
                     `4` = "Employment (salaried income)",
                     `5` = "Sale of personal assets",
                     `6` = "Remittance",
                     `7` = "Other")
  )

# Check unique values for verification
unique(final_data$reason_for_ineligibility)
unique(final_data$rspntgndr)
unique(final_data$h_hfrml_eductn)
unique(final_data$rspndtmarital)
unique(final_data$rspndt_eductn)
unique(final_data$maincme)

unique(final_data$lvstckown)

install.packages("tidyr")
library(tidyr)


# Separate the lvstckown column into individual species columns
final_data <- final_data %>%
  separate(lvstckown, into = paste0("animal_", 1:15), sep = " ", fill = "right", remove = TRUE)

unique(final_data$herdynamics)

# Separate the herdynamics column into individual response columns
final_data <- final_data %>%
  separate(herdynamics, into = paste0("herdynamics_", 1:7), sep = " ", fill = "right", remove = TRUE)

# Display the first few rows of the updated dataset
head(final_data)

# Check the column names in final_data
colnames(final_data)


# Create the new study_arm column based on village names
final_data <- final_data %>%
  mutate(study_arm = case_when(
    village.x %in% c("Lependera", "Gobb Arbelle", "Nahgan-ngusa", "Sulate", 
                     "Saale-Sambakah", "Namarei", "Manyatta Lengima", "Lokoshula", 
                     "TubchaDakhane", "Rengumo-Gargule") ~ "Study arm 1",
    village.x %in% c("Galthelian-Torrder", "Uyam village", "Galthelan Elemo", 
                     "Nebey", "Rongumo_kurkum", "Urawen_Kurkum", "Eisimatacho", 
                     "Manyatta K.A.G", "Ltepes Ooodo", "Lorokushu", "Marti", 
                     "Manyatta Juu West/East", "Lbaarok1") ~ "Study arm 2",
    TRUE ~ "Study arm 3"  # All other villages
  ))

# Check the column names in final_data
colnames(final_data)

unique(final_data$study_arm)

# Create herd_dynamics object with specified columns
herd_dynamics <- final_data %>%
  select(interview_date = interview_date.x, 
         household_id, 
         study_arm, 
         cwsbrth, 
         shpbrth, 
         goatsbrth, 
         cmlsbrth, 
         calves_death, 
         bulls_death, 
         cows_death, 
         sheep_death, 
         msheep_death, 
         fsheep_death, 
         goats_death, 
         mgoats_death, 
         fgoats_death, 
         camels_death, 
         mcamels_death, 
         fcamels_death, 
         cowsgft, 
         sheepgfts, 
         goatsgft, 
         cmlsgft,
         cowsgvnout,       
         sheepgvnout,       
         goatsgvnout,       
         cmlsgvnout)

# Create monthyear column
herd_dynamics <- herd_dynamics %>%
  mutate(monthyear = format(ymd(interview_date), "%Y-%m"))  # Convert interview_date to Date format

# Replace "---" with NA and convert to numeric for specific columns
herd_dynamics <- herd_dynamics %>%
  mutate(across(c(cwsbrth, shpbrth, goatsbrth, cmlsbrth, 
                  calves_death, bulls_death, cows_death, 
                  sheep_death, msheep_death, fsheep_death, 
                  goats_death, mgoats_death, fgoats_death, 
                  camels_death, mcamels_death, fcamels_death, 
                  cowsgft, sheepgfts, goatsgft, cmlsgft,cowsgvnout,sheepgvnout,goatsgvnout, cmlsgvnout  ), 
                ~ as.numeric(ifelse(. == "---", NA, .))))

# Check the structure to confirm the changes
str(herd_dynamics)

# Calculate the number of animals born, died, gifted, and given out
animal_summary <- herd_dynamics %>%
  group_by(study_arm, monthyear) %>%
  summarize(
    cows_born = sum(cwsbrth, na.rm = TRUE),
    sheep_born = sum(shpbrth, na.rm = TRUE),
    goats_born = sum(goatsbrth, na.rm = TRUE),
    camels_born = sum(cmlsbrth, na.rm = TRUE),
    cows_died = sum(cows_death, na.rm = TRUE),
    sheep_died = sum(sheep_death,msheep_death, fsheep_death, na.rm = TRUE),
    goats_died = sum(goats_death, mgoats_death,fgoats_death, na.rm = TRUE),
    camels_died = sum(camels_death, mcamels_death, fcamels_death, na.rm = TRUE),
    cows_gifted = sum(cowsgft, na.rm = TRUE),
    sheep_gifted = sum(sheepgfts, na.rm = TRUE),
    goats_gifted = sum(goatsgft, na.rm = TRUE),
    camels_gifted = sum(cmlsgft, na.rm = TRUE),
    cows_given = sum(cowsgvnout, na.rm = TRUE),     
    sheep_given = sum(sheepgvnout, na.rm = TRUE),   
    goats_given = sum(goatsgvnout, na.rm = TRUE),
    camels_given = sum(cmlsgvnout, na.rm = TRUE), 
    .groups = 'drop'  # Ungroup after summarizing
  )

print(animal_summary)

# Create a subset of the dataset with the specified variables
subset_herd_dynamics <- animal_summary %>%
  select(
    study_arm, 
    monthyear, 
    cows_born, 
    sheep_born, 
    goats_born, 
    camels_born, 
    cows_died, 
    sheep_died, 
    goats_died, 
    camels_died, 
    cows_gifted, 
    sheep_gifted, 
    goats_gifted, 
    camels_gifted, 
    cows_given, 
    sheep_given, 
    goats_given, 
    camels_given
  ) %>%
  distinct()  # Remove duplicates

# Check the structure of the new subset
str(subset_herd_dynamics)

summary(subset_herd_dynamics)

View(subset_herd_dynamics)


# Print the first few rows of the subset to verify
head(subset_herd_dynamics)

install.packages("ggplot2")
library(ggplot2)

# Reshape the data to long format
long_data <- subset_herd_dynamics %>%
  pivot_longer(
    cols = c(cows_born, cows_died, cows_gifted, cows_given,
             sheep_born, sheep_died, sheep_gifted, sheep_given,
             goats_born, goats_died, goats_gifted, goats_given,
             camels_born, camels_died, camels_gifted, camels_given),
    names_to = c("species", "event"),
    names_pattern = "(.*)_(.*)",
    values_to = "count"
  ) %>%
  mutate(event = recode(event,
                        'born' = 'Births',
                        'died' = 'Deaths',
                        'gifted' = 'Gifts In',
                        'given' = 'Gifts Out')) %>%
  drop_na(count)  # Remove NA counts for cleaner plotting

# Create the plot
ggplot(long_data, aes(x = monthyear, y = count, fill = species)) +
  geom_col(position = "dodge") +  # Use dodge for side-by-side bars
  facet_grid(study_arm ~ event, scales = "free_y") +  # Create separate panels for each study arm and event
  labs(title = "Frequencies and Changes Over Time in Animal Events",
       x = "Month-Year",
       y = "Count") +
  scale_fill_manual(values = c("cows" = "blue", 
                               "sheep" = "green", 
                               "goats" = "purple", 
                               "camels" = "orange")) +  # Custom colors for species
  theme_minimal() +  # Clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability






