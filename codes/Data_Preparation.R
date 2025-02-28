###############################################
#           Data Preparation Script           #
#   Analysis of Urban NTL Response to Heatwaves #
###############################################

# Load required libraries
library(dplyr)
library(tidyr)
library(stats)

###############################################
# 1. Load the Dataset for Each City
###############################################

# For a non-pooled analysis, we consider one city at a time.
# Uncomment the dataset you want to analyze:

# Guangzhou:
# data <- read.csv("Guangzhou_NTL(Low).xlsx - Sheet2 (1) - Guangzhou_NTL.xlsx - Sheet2 (1) (2).csv")

# Delhi:
# data <- read.csv("Delhi 2012-01-01 to 2024-01-01 - Sheet2.csv")

# São Paulo:
# data <- read.csv("SauPalo_NTL.xlsx - Sheet2 (1).csv")

# Cairo:
data <- read.csv("Cairo_N - Sheet1.csv")

###############################################
# 2. Data Cleaning Step 1: Remove Null/Zero NTL
###############################################

# Remove rows where Mean.Area.Weighted.Radiance is 0 (assumed to be null or non-informative)
data <- data %>% 
  filter(Mean.Area.Weighted.Radiance != 0)

###############################################
# 3. Define Heatwave Thresholds for Each City
###############################################

# Set the temperature threshold based on local climate conditions.
# Note: Adjust these percentiles as needed for each city.
# For Guangzhou & Cairo, we use the 80th percentile;
# For Delhi, use the 85th percentile;
# For São Paulo, use the 90th percentile.

# Example for Guangzhou & Cairo:
heatwave_threshold <- quantile(data$temp, probs = 0.80, na.rm = TRUE)
# For Delhi, uncomment the following line:
# heatwave_threshold <- quantile(data$temp, probs = 0.85, na.rm = TRUE)
# For São Paulo, uncomment the following line:
# heatwave_threshold <- quantile(data$temp, probs = 0.90, na.rm = TRUE)

###############################################
# 4. Define Heatwaves Based on Consecutive Hot Days
###############################################

# We define a "hot day" as one where the temperature is above the threshold.
# Then, we identify heatwave events as periods with consecutive hot days.
# We provide examples for different definitions: 2+ days, 3+ days, and 4+ days.
# Uncomment the block corresponding to your analysis.

# Option A: Heatwave defined as 2+ consecutive hot days
data <- data %>%
  mutate(
    is_hot_day = ifelse(temp >= heatwave_threshold, 1, 0),
    heatwave_flag = as.numeric(stats::filter(is_hot_day, rep(1, 2), sides = 1) >= 2)
  )

# Option B: Heatwave defined as 3+ consecutive hot days
data <- data %>%
  mutate(
    is_hot_day = ifelse(temp >= heatwave_threshold, 1, 0),
    heatwave_flag = as.numeric(stats::filter(is_hot_day, rep(1, 3), sides = 1) >= 3)
  )

# Option C: Heatwave defined as 4+ consecutive hot days
data <- data %>%
  mutate(
    is_hot_day = ifelse(temp >= heatwave_threshold, 1, 0),
    heatwave_flag = as.numeric(stats::filter(is_hot_day, rep(1, 4), sides = 1) >= 4)
  )

# Note: Only one of these options should be active at a time. 
# Choose the definition that best suits your analysis.

###############################################
# 5. Data Cleaning Step 2: Finalize Heatwave Indicator
###############################################

# Replace any NA values in heatwave_flag with 0.
data$heatwave_flag[is.na(data$heatwave_flag)] <- 0

# Create a final binary indicator for heatwave days.
data$is_heatwave <- data$heatwave_flag

# Optional: Get total number of heatwave events in the dataset.
total_heatwave_days <- sum(data$is_heatwave)
print(paste("Total heatwave days:", total_heatwave_days))

###############################################
# 6. Create Additional Variables and Lagged Terms
###############################################

# Cooling Degree Days (CDD): the excess temperature above the threshold (0 if below threshold)
data$CDD <- ifelse(data$temp >= heatwave_threshold, data$temp - heatwave_threshold, 0)

# Interaction between heatwave indicator and humidity
data$heatwave_humidity_interaction <- data$is_heatwave * data$humidity

# Interaction between heatwave indicator and solar energy
data <- data %>%
  mutate(heatwave_flag_solarenergy = is_heatwave * solarenergy)

# Interaction between maximum temperature and cloud cover
data <- data %>%
  mutate(tempmax_cloudcover = tempmax * cloudcover)

# Create lag variables (lag of 1, 2, and 3 days for temperature; similar for CDD, humidity, cloudcover)
data$temp_Lag1 <- lag(data$temp, 1)
data$temp_Lag2 <- lag(data$temp, 2)
data$temp_Lag3 <- lag(data$temp, 3)

data$CDD_Lag1 <- lag(data$CDD, 1)
data$CDD_Lag2 <- lag(data$CDD, 2)
data$CDD_Lag3 <- lag(data$CDD, 3)

data$humidity_Lag1 <- lag(data$humidity, 1)
data$humidity_Lag2 <- lag(data$humidity, 2)

data$cloudcover_Lag1 <- lag(data$cloudcover, 1)

# Remove rows with missing values in lagged variables (if necessary)
data <- data %>% filter(!is.na(CDD_Lag3))

###############################################
# End of Data Preparation Script
###############################################
