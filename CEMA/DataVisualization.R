# Install packages if you haven't already
install.packages(c("sf", "tmap"))
install.packages("viridis")

# Load the libraries
library(dplyr)
library(ggplot2)
library(sf)
library(tmap)
library(viridis)

# Load the teenage pregnancy data
teen_pregnancy_data <- read.csv("https://raw.githubusercontent.com/cema-uonbi/L4H_sample_data/main/table6_teenpregnancybycounty.csv")

# Read the shapefiles into R
shapefile_path <- "C:/Users/Anarchy/Documents/Data_Science/CEMA/L4H_sample_data-main/shapefiles/County.shp"
kenya_map <- st_read(shapefile_path) 

# Check the structure of both datasets to ensure county names match
str(teen_pregnancy_data)
str(kenya_map)

# Merge the datasets by county
kenya_merged <- kenya_map %>%
  left_join(teen_pregnancy_data, by = c("Name" = "County"))  # Adjusting to match column names
# Check the structure of the merged dataset
str(kenya_merged)

# Check for any NA values in the merged dataset
summary(kenya_merged)

# Handle missing values in the Ever_pregnant column
kenya_merged <- kenya_merged %>%
  mutate(Ever_pregnant = ifelse(is.na(Ever_pregnant), 0, Ever_pregnant))

# Check for any NA values in the merged dataset
summary(kenya_merged)

# Create a map showing the percentage of teenagers who have ever been pregnant by county
ggplot(data = kenya_merged) +
  geom_sf(aes(fill = Ever_pregnant), color = "white") +  # Fill by Ever_pregnant
  scale_fill_viridis(option = "plasma", name = "Percentage of Teenagers Ever Pregnant") +  # Customize color scale
  labs(title = "Percentage of Teenagers Who Have Ever Been Pregnant by County in Kenya (2022)",
       subtitle = "Data from the Kenya Demographic Health Survey",
       caption = "Source: Kenya Demographic Health Survey") +
  theme_minimal() +
  theme(legend.position = "bottom")












