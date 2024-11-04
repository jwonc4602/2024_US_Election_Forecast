#### Preamble ####
# Purpose: Cleans the raw presidential polling data and prepares it for state-level and national Electoral College analysis
# Author: Jiwon Choi and Kevin Roe
# Date: 10 October 2024
# Contact: jwon.choi@mail.utoronto.ca and kevin.roe@mail.utoronto.ca
# License: MIT
# Pre-requisites: download the raw dataset

#### Workspace setup ####
library(tidyverse)
library(janitor)
library(lubridate)

#### Clean data ####
# Load the raw data
raw_data <- read_csv("data/01-raw_data/president_polls.csv")

# Clean and select relevant variables, and focus on Trump and Harris
cleaned_data <- 
  raw_data |>
  clean_names() |>
  select(
    pollster, numeric_grade, state, candidate_name, party, pct, sample_size, 
    population, start_date, end_date
  ) |>
  # Drop rows where numeric_grade is NA
  drop_na(numeric_grade) |>
  # Filter for Trump and Harris polls only
  filter(candidate_name %in% c("Donald Trump", "Kamala Harris")) |>
  # Convert "--" in state column to NA (non-national polls) and label as national if NA
  mutate(
    state = if_else(state == "--", NA_character_, state),
    national_poll = if_else(is.na(state), 1, 0)  # 1 for national, 0 for state-specific
  ) |>
  # Combine "Maine", "Maine CD-1", and "Maine CD-2" into "Maine"
  # Combine "Nebraska" and "Nebraska CD-2" into "Nebraska"
  mutate(
    state = case_when(
      state %in% c("Maine", "Maine CD-1", "Maine CD-2") ~ "Maine",
      state %in% c("Nebraska", "Nebraska CD-1", "Nebraska CD-2", "Nebraska CD-3") ~ "Nebraska",
      TRUE ~ state  # Keep other states unchanged
    )
  ) |>
  # Convert polling percentage and sample size to numeric
  mutate(
    pct = as.numeric(pct),
    sample_size = as.numeric(sample_size)
  ) |>
  # Standardize the population column based on sample types
  mutate(
    population = case_when(
      str_detect(population, regex("A", ignore_case = TRUE)) ~ "Adults",
      str_detect(population, regex("V", ignore_case = TRUE)) & population != "LV" & population != "RV" ~ "Voters", 
      str_detect(population, regex("LV", ignore_case = TRUE)) ~ "Likely Voters",
      str_detect(population, regex("RV", ignore_case = TRUE)) ~ "Registered Voters",
      TRUE ~ population  # For any other values not matching A, V, LV, or RV
    )
  ) |>
  # Clean candidate names (e.g., removing trailing/leading spaces)
  mutate(candidate_name = str_trim(candidate_name)) |>
  # Convert dates to a consistent date format using lubridate
  mutate(
    start_date = mdy(start_date),
    end_date = mdy(end_date)
  ) |>
  # Create an indicator for poll recency while keeping start_date and end_date
  mutate(
    recent_poll = if_else(end_date >= Sys.Date() - 30, "Recent", "Older")
  ) |>
  # Remove all rows with any NA values except for the state (which is already handled)
  drop_na(pct, sample_size, candidate_name, pollster)

# Export cleaned datasets
write_csv(cleaned_data, "data/02-analysis_data/cleaned_president_polls.csv")

