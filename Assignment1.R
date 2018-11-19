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

# Part 2 - Q1) -----------------------------------------------------
forbes_raw <- read.csv("forbes.csv")
forbes_raw$rank <- parse_number(forbes_raw$rank)
forbes_raw$net_worth <- parse_number(forbes_raw$net_worth)
forbes_raw$net_worth <- as.numeric(as.character(forbes_raw$net_worth))

# Part 2 - Q2) ------------------------------------------------------------
# In this question, I have filtered the data by the bellow function.
# I have notices that there are a number of rows with 3 digit net worth.
# These can be deleted to prevent distortion in our data.
forbes_filter <- filter(forbes_raw, net_worth < 950)

# Part 2 - Q3) ------------------------------------------------------------
ggplot(data = forbes_filter) +
  geom_point(mapping = aes(x = age, y = net_worth))

# Now the same question with log(net_worth)
ggplot(data = forbes_filter) +
  geom_point(mapping = aes(x = age, y = log(net_worth)))

# Considering the log of net_worth, the density of the points is better drawn.
# ...
