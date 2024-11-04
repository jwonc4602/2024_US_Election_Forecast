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
cleaned_president_polls <- read_csv("data/02-analysis_data/cleaned_president_polls.csv")
state_data <- read_csv("data/02-analysis_data/state_polling_data.csv")

#### Data filtering ####
# Filter data to Harris estimates based on high-quality polls after she declared
filtered_data <- cleaned_president_polls |>
  filter(
    candidate_name == "Kamala Harris",
    numeric_grade >= 3.0
  ) |>
  filter(end_date >= as.Date("2024-07-21")) # When Harris declared

#### Model: Vote Prediction ####
vote_for_Harris <- stan_glm(
  formula = pct ~ pollster + population + recent_poll,
  data = filtered_data,
  family = gaussian(),
  prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
  prior_intercept = normal(location = 0, scale = 2.5, autoscale = TRUE),
  prior_aux = exponential(rate = 1, autoscale = TRUE),
  seed = 853,
  iter = 4000,
  warmup = 2000
)

# Summarize and save the popular vote model
summary(vote_for_Harris)
write_rds(vote_for_Harris, "models/vote_for_Harris_model.rds")

#### Predictions: Popular Vote ####
# Predicting the popular vote percentages
popular_vote_prediction <- predict(vote_for_Harris, newdata = filtered_data)
filtered_data <- filtered_data |>
  mutate(predicted_pct = popular_vote_prediction)


# Save predictions
write_csv(filtered_data, "data/02-analysis_data/vote_for_Harris_predictions.csv")

