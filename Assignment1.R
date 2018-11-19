# Assignment

# Inserting Libraries ----------------------------------------
library(tidyverse)
library(tidyr)
library(tidyselect)
library(euR)
library(dplyr)
library(ggplot2)
library(GGally)
library(proto)
library(sqldf)
library(gsubfn)
library(forcats)
library(microbenchmark)
library(rmarkdown)
library(colorspace)
library(readr)

# Part 2 -----------------------------------------------------
## Question 1
forbes_raw <- read.csv("forbes.csv")
forbes_raw$rank <- parse_number(forbes_raw$rank)
forbes_raw$net_worth <- parse_number(forbes_raw$net_worth)