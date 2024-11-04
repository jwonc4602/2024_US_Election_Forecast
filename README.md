# 2024 US Election Forecast

## Overview

This repo contains the data, code, and analysis of the 2024 U.S. presidential election forecast, designed by Jiwon Choi and Kevin Roe. Parquet file was not able to be used due to unresolved technical issues.

## File Structure

The repo is structured as:

-   `data/00-simulated_data` contains the simulated data.
-   `data/01-raw_data` contains the raw data as obtained from [fivethirtyeight](https://projects.fivethirtyeight.com/polls/president-general/2024/national/).
-   `data/02-analysis_data` contains the cleaned dataset that was constructed.
-   `model` contains fitted generalized linear models.
-   `other` contains sketches.
-   `paper` contains the files used to generate the paper, including the Quarto document and reference bibliography file, as well as the PDF of the paper.
-   `scripts` contains the R scripts used to simulate, clean, and test data. Also, the folder contains R script for data modeling.

## Raw Data details

You can get data about polling outcomes from [fivethirtyeight](https://projects.fivethirtyeight.com/polls/president-general/2024/national/). To download the data, search for “Download the data”, then select Presidential general election polls (current cycle), then “Download”.

## Statement on LLM usage

*None of the components of this work involved the use of LLMs.* 
