#### Preamble ####
# Purpose: Tests for cleaning the 'cleaned_president_polls.csv' dataset
# Author: Jiwon Choi and Kevin Roe
# Date: 14 October 2024
# Contact: jwon.choi@mail.utoronto.ca and kevin.roe@mail.utoronto.ca
# License: MIT
# Pre-requisites: 03-clean_data.R
# Any other information needed? None.


#### Workspace setup ####
library(tidyverse)
library(testthat)

#### Setup ####
# Load the dataset globally (this should remain outside the test_that block)
data <- read_csv("../data/02-analysis_data/cleaned_president_polls.csv")

#### Test data ####

# Test that the dataset has 11 columns
test_that("dataset has 11 columns", {
  expect_equal(ncol(data), 12)
})

# Test that 'pollster' column is character type
test_that("'pollster' is character", {
  expect_type(data$pollster, "character")
})

# Test that 'numeric_grade' column is numeric type
test_that("'numeric_grade' is numeric", {
  expect_type(data$numeric_grade, "double")
})

# Test that 'state' column is character and contains valid state names or NaN for national polls
valid_states <- c(
  "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", 
  "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", 
  "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", 
  "Maine", "Maryland", "Massachusetts", 
  "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", 
  "Nebraska", "Nevada", "New Hampshire", "New Jersey", 
  "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", 
  "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", 
  "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", 
  "Washington", "West Virginia", "Wisconsin", "Wyoming", NA
)
test_that("'state' contains valid US state names or NaN", {
  expect_true(all(is.na(data$state) | data$state %in% valid_states))
})

# Test that 'pct' is a numeric column and falls between 0 and 100
test_that("'pct' values are between 0 and 100", {
  expect_true(all(data$pct >= 0 & data$pct <= 100))
})

# Test that 'sample_size' is an integer and greater than 0
test_that("'sample_size' is a positive integer", {
  expect_true(all(data$sample_size > 0))
})

# Test that 'population' has no empty strings
test_that("no empty strings in 'population' column", {
  expect_false(any(data$population == ""))
})

# Test that 'recent_poll' contains only valid values ('Recent' or 'Older')
valid_recent_values <- c("Recent", "Older")
test_that("'recent_poll' contains valid values", {
  expect_true(all(data$recent_poll %in% valid_recent_values))
})

# Test that 'national_poll' is a binary column (0 or 1)
test_that("'national_poll' is binary", {
  expect_true(all(data$national_poll %in% c(0, 1)))
})
