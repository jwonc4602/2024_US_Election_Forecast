#### Preamble ####
# Purpose: Simulates a dataset of U.S. electoral divisions (states), including the state and party that won each state.
# Author: Jiwon Choi and Kevin Roe
# Date: 24 October 2024
# Contact: jwon.choi@mail.utoronto.ca and kevin.roe@mail.utoronto.ca
# License: MIT
# Pre-requisites: The `tidyverse` package must be installed
# Any other information needed? None.


#### Workspace setup ####
library(tidyverse)
set.seed(2024)


#### Simulate data ####
# State names (all 50 states + Washington, D.C.)
states <- c(
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

# Political parties
parties <- c("Democratic", "Republican", "Other")

# Adjust probabilities for state populations and party distribution.
# For party distribution, we can roughly estimate based on recent election results.
# For example, we might estimate Democrats and Republicans having near-equal chances
# in most states, with some minor parties or independents as well.
state_probs <- c(
  0.02, 0.01, 0.02, 0.01, 0.12, 0.02, 0.01, 0.005, 0.06, 0.04, 
  0.01, 0.005, 0.04, 0.02, 0.01, 0.01, 0.02, 0.02, 0.005, 0.02, 
  0.02, 0.04, 0.02, 0.01, 0.02, 0.005, 0.01, 0.01, 0.005, 0.03, 
  0.005, 0.07, 0.04, 0.005, 0.04, 0.01, 0.02, 0.05, 0.005, 0.02, 
  0.005, 0.03, 0.10, 0.02, 0.005, 0.04, 0.02, 0.005, 0.03, 0.005, 0.005
)

party_probs <- c(0.45, 0.45, 0.10) # Rough estimate for Democratic, Republican, and Other parties

# Create a dataset by randomly assigning states and parties to each state
analysis_data <- tibble(
  state = states,
  party = sample(
    parties,
    size = length(states),
    replace = TRUE,
    prob = party_probs
  )
)

#### Save data ####
write_csv(analysis_data, "data/00-simulated_data/us_2024_simulated_data.csv")

