#### Preamble ####
# Purpose: Models forecast popular vote and state winners (electoral votes)
# Author: Jiwon Choi and Kevin Roe
# Date: 12 October 2024
# Contact: jwon.choi@mail.utoronto.ca and kevin.roe@mail.utoronto.ca
# License: MIT
# Pre-requisites: Cleaned data available in respective CSV files

#### Workspace setup ####
library(tidyverse)
library(rstanarm)

#### Read data ####
# Load the cleaned data for modeling
national_data <- read_csv("data/02-analysis_data/national_polling.csv")

#### Data filtering ####
# Filter data to Harris estimates based on high-quality polls after she declared
filtered_national_data <- national_data |>
  filter(
    candidate_name == "Kamala Harris",
    numeric_grade >= 2.7 # Need to investigate this choice - come back and fix. 
    # Also need to look at whether the pollster has multiple polls or just one or two - filter out later
  ) |>
  filter(end_date >= as.Date("2024-07-21")) # When Harris declared

#### Model 1: Popular Vote Prediction ####
popular_vote_model <- stan_glm(
  formula = pct ~ + pollster + population + recent_poll,
  data = filtered_national_data,
  family = gaussian(),
  prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
  prior_intercept = normal(location = 0, scale = 2.5, autoscale = TRUE),
  prior_aux = exponential(rate = 1, autoscale = TRUE),
  seed = 853,
  iter = 4000,
  warmup = 2000
)

# Summarize and save the popular vote model
summary(popular_vote_model)
write_rds(popular_vote_model, "models/popular_vote_model.rds")


#### Predictions: Popular Vote ####
# Predicting the popular vote percentages for each candidate
popular_vote_prediction <- predict(popular_vote_model, newdata = filtered_national_data)
filtered_national_data <- filtered_national_data |>
  mutate(predicted_pct = popular_vote_prediction)

# Save popular vote predictions
write_csv(filtered_national_data, "data/02-analysis_data/popular_vote_predictions.csv")
