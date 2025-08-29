###############################################
#           Data Preparation Script           #
#   Analysis of Urban NTL Response to Heatwaves
###############################################

# Load required libraries
library(dplyr)
library(tidyr)
library(stats)
library(lubridate)   

###############################################
# 1) Load ONE city dataset
###############################################


# Guangzhou:
# data <- read.csv("/Users/taranchandel/Downloads/Guangzhou")

# Delhi:
# data <- read.csv("/Users/taranchandel/Downloads/Delhi")

# São Paulo:
# data <- read.csv("/Users/taranchandel/Downloads/SaoPalo")

# Cairo:
data <- read.csv("/Users/taranchandel/Downloads/Cairo_FTL")

###############################################
# 2) Basic cleaning and quick sanity checks
###############################################

# Drop rows with zero NTL (assumed to be null / non-informative)
data <- data %>% filter(Mean.Area.Weighted.Radiance != 0)

# Optional quick summaries
summary(data$Mean.Area.Weighted.Radiance)
summary(data$temp)

###############################################
# 3) City-specific hot-day threshold (95th pct)
###############################################
# Two common variants are shown; keep ONE active.

## (A) Use only warm-season months - Different for each of our city
#Delhi - 4,5,6,7
#São Paulo - 12,1,2,3
#Guangzhou = 6,7,8,9
#Cairo -  6,7,8,9
# heatwave_threshold <- data %>%
#   filter(month(datetime) %in% c(4, 5, 6, 7)) %>%     # adjust months to your context
#   summarise(thr95 = quantile(temp, 0.95, na.rm = TRUE)) %>%
#   pull(thr95)





###############################################
# 4) Define heat-waves (consecutive hot days)
###############################################
# We first flag a "hot day", then detect runs of consecutive hot days.
# Choose exactly ONE definition below (d = 2, 3, or 4).



## Option A: d >= 2 consecutive hot days
# data <- data %>%
#   mutate(heatwave_flag = as.numeric(stats::filter(is_hot_day, rep(1, 2), sides = 1) >= 2))

## Option B: d >= 3 consecutive hot days  (← example active)
data <- data %>%
  mutate(heatwave_flag = as.numeric(stats::filter(is_hot_day, rep(1, 3), sides = 1) >= 3))

## Option C: d >= 4 consecutive hot days
# data <- data %>%
#   mutate(heatwave_flag = as.numeric(stats::filter(is_hot_day, rep(1, 4), sides = 1) >= 4))

# Finalize binary indicator (replace any NA from the rolling filter with 0)
data$heatwave_flag[is.na(data$heatwave_flag)] <- 0
data$is_heatwave <- data$heatwave_flag

# Quick count
total_heatwave_days <- sum(data$is_heatwave, na.rm = TRUE)
print(paste("Total heat-wave days:", total_heatwave_days))

###############################################
# 5) Derived variables & interactions
###############################################

# Cooling Degree Days (relative to the chosen threshold)
data$CDD <- ifelse(data$temp >= heatwave_threshold, data$temp - heatwave_threshold, 0)

# Heat-wave × humidity
data$heatwave_humidity_interaction <- data$is_heatwave * data$humidity

# Heat-wave × solar energy
data <- data %>%
  mutate(heatwave_flag_solarenergy = is_heatwave * solarenergy)

# Tmax × cloud cover
data <- data %>%
  mutate(tempmax_cloudcover = tempmax * cloudcover)

###############################################
# 6) Lags (adjust horizons as needed)
###############################################

# Temperature lags
data$temp_Lag1 <- lag(data$temp, 1)
data$temp_Lag2 <- lag(data$temp, 2)
# add more if you need
# data$temp_Lag3 <- lag(data$temp, 3)

# CDD lags
data$CDD_Lag1 <- lag(data$CDD, 1)
data$CDD_Lag2 <- lag(data$CDD, 2)
# data$CDD_Lag3 <- lag(data$CDD, 3)

#  other lags commonly used
data$humidity_Lag1    <- lag(data$humidity, 1)
data$cloudcover_Lag1  <- lag(data$cloudcover, 1)
 

##Heat Index- 
#Heat Index (HI): the apparent temperature in °C that combines air temperature and relative humidity, 
#computed with the NOAA/Steadman simplified heat-index formula

T_F <- data$temp * 9/5 + 32        # Celsius → Fahrenheit
RH  <- data$humidity               # already in %

HI_F <- 0.5*(T_F + 61 + (T_F - 68)*1.2 + RH*0.094)
data$HI <- (HI_F - 32) * 5/9       # back to °C 
data$HI_Lag1          <- lag(data$HI, 1)  

# Remove rows with missing values in lagged variables (if necessary)
data <- data %>% filter(!is.na(CDD_Lag2))


# hw_humAdj: interaction of the heat-wave indicator with temperature-orthogonalized lagged humidity, 
# capturing the part of humidity (net of temperature) that matters on heat-wave days.
# 1. Fit the regression of the lagged humidity on lagged temperature
mod_hum <- lm(humidity_Lag1 ~ temp_Lag1, data = data)

# 2. Extract the residuals from that model
hum_resid <- resid(mod_hum)

# 3. Add the residuals to your data
data$hum_adj <- hum_resid # Covariate of interest 


