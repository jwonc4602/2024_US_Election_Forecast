#### Preamble ####
# Purpose: Models the support for Kamala Harris using high-quality pollsters
# Author: Jiwon Choi
# Date: 12 October 2024
# Contact: jwon.choi@mail.utoronto.ca
# License: MIT
# Pre-requisites: 03-cleaned_data.R

#### Workspace setup ####
library(tidyverse)
library(rstanarm)

#### Read data ####
# Load the cleaned data for modeling
cleaned_data <- read_csv("data/02-analysis_data/cleaned_president_polls.csv")

# Load the cleaned national and state data for making predictions
national_polling_data <- read_csv("data/02-analysis_data/national_polling_data.csv")
state_polling_data <- read_csv("data/02-analysis_data/state_polling_data.csv")

#### Data filtering for the model ####
# Filter to high-quality pollsters (numeric_grade >= 2.2) and only Harris  
analysis_data <- cleaned_data |>
  filter(numeric_grade >= 3, candidate_name == "Kamala Harris")
cleaned_national_data <- national_polling_data |>
  filter(numeric_grade >= 3, candidate_name == "Kamala Harris")
cleaned_state_data <- state_polling_data |>
  filter(numeric_grade >= 3, candidate_name == "Kamala Harris")

### Model support for Harris ####
# Model Harris' support as a function of pollster, national/state poll, and recency
harris_model <- stan_glm(
  formula = pct ~ pollster + recent_poll + population,  # Include national_poll as a predictor
  data = analysis_data,
  family = gaussian(),
  prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
  prior_intercept = normal(location = 0, scale = 2.5, autoscale = TRUE),
  prior_aux = exponential(rate = 1, autoscale = TRUE),
  seed = 853,
  iter = 4000,  # Default is usually 2000
  warmup = 2000 # Half of iter is a common choice
)

#### Save model ####
saveRDS(
  harris_model,
  file = "models/harris_model.rds"
)

#### Model results ####
print(summary(harris_model))

#### Predictions ####
# Use the model to make predictions for both national and state data

# Prediction for national polls
national_predicted_support <- predict(harris_model, newdata = cleaned_national_data)
cleaned_national_data <- cleaned_national_data |>
  mutate(predicted_pct = national_predicted_support)

# Prediction for state polls
state_predicted_support <- predict(harris_model, newdata = cleaned_state_data)
cleaned_state_data <- cleaned_state_data |>
  mutate(predicted_pct = state_predicted_support)

#### Save predictions ####
write_csv(cleaned_national_data, "data/02-analysis_data/national_predicted_support.csv")
write_csv(cleaned_state_data, "data/02-analysis_data/state_predicted_support.csv")

