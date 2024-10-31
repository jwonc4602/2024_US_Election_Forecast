#### Preamble ####
# Purpose: Tests the structure and validity of the simulated U.S. electoral divisions dataset.
# Author: Jiwon Choi and Kevin Roe
# Date: 30 October 2024
# Contact: jwon.choi@mail.utoronto.ca and kevin.roe@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - The `tidyverse` package must be installed and loaded
# - 00-simulate_data.R must have been run
# Any other information needed? None.


#### Workspace setup ####
library(tidyverse)

# Load the U.S. electoral dataset
analysis_data <- read_csv("data/00-simulated_data/us_2024_simulated_data.csv")

# Test if the data was successfully loaded
if (exists("analysis_data")) {
  message("Test Passed: The dataset was successfully loaded.")
} else {
  stop("Test Failed: The dataset could not be loaded.")
}


#### Test data ####

# Check if the dataset has 51 rows (50 states + Washington, D.C.)
if (nrow(analysis_data) == 51) {
  message("Test Passed: The dataset has 51 rows.")
} else {
  stop("Test Failed: The dataset does not have 51 rows.")
}

# Check if the dataset has 2 columns
if (ncol(analysis_data) == 2) {
  message("Test Passed: The dataset has 2 columns.")
} else {
  stop("Test Failed: The dataset does not have 2 columns.")
}

# Check if all values in the 'state' column are unique
if (n_distinct(analysis_data$state) == nrow(analysis_data)) {
  message("Test Passed: All values in 'state' are unique.")
} else {
  stop("Test Failed: The 'state' column contains duplicate values.")
}

# Check if the 'state' column contains only valid U.S. state names
valid_states <- c(
  "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", 
  "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", 
  "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", 
  "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", 
  "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", 
  "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", 
  "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", 
  "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", 
  "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming",
  "District of Columbia"
)

if (all(analysis_data$state %in% valid_states)) {
  message("Test Passed: The 'state' column contains only valid U.S. state names.")
} else {
  stop("Test Failed: The 'state' column contains invalid state names.")
}

# Check if the 'party' column contains only valid party names
valid_parties <- c("Democratic", "Republican", "Other")

if (all(analysis_data$party %in% valid_parties)) {
  message("Test Passed: The 'party' column contains only valid party names.")
} else {
  stop("Test Failed: The 'party' column contains invalid party names.")
}

# Check if there are any missing values in the dataset
if (all(!is.na(analysis_data))) {
  message("Test Passed: The dataset contains no missing values.")
} else {
  stop("Test Failed: The dataset contains missing values.")
}

# Check if there are no empty strings in 'state' and 'party' columns
if (all(analysis_data$state != "" & analysis_data$party != "")) {
  message("Test Passed: There are no empty strings in 'state' or 'party'.")
} else {
  stop("Test Failed: There are empty strings in one or more columns.")
}

# Check if the 'party' column has at least two unique values
if (n_distinct(analysis_data$party) >= 2) {
  message("Test Passed: The 'party' column contains at least two unique values.")
} else {
  stop("Test Failed: The 'party' column contains less than two unique values.")
}

