#' Title: Assignment 1 - R Programming Course
#' Author: Mohamadreza Hoseinpour

# Loading Libraries ----------------------------------------
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
library(styler)
library(rworldmap)

# Part 2 - Q1) -----------------------------------------------------
forbes_raw <- read.csv("forbes.csv")
forbes_raw$rank <- parse_number(forbes_raw$rank)
forbes_raw$net_worth <- parse_number(forbes_raw$net_worth)
forbes_raw$age <- parse_number(forbes_raw$age)
length(which(is.na(forbes_raw$age)))
# After doing so, we can see that 39 rows are missing because of NAs in age.

# Part 2 - Q2) ------------------------------------------------------------
{
  # In this question, I have filtered the data by the bellow function.
  # I have noticed that there are a number of rows with 3 digit net worth.
  # These can be deleted to prevent distortion in our data.
}
forbes_filter <- filter(forbes_raw, net_worth < 950)

# Part 2 - Q3) ------------------------------------------------------------
ggplot(data = forbes_filter) +
  geom_point(mapping = aes(x = age, y = net_worth))

# Now the same question with log(net_worth)
ggplot(
  data = forbes_filter,
  mapping = aes(x = age, y = log(net_worth))
) +
  geom_point() +
  geom_smooth()
{
  # Considering the log of net_worth, the density of the points is better drawn.
  # We can see the density of data more clearly.
  }

# Part 2 - Q4) ------------------------------------------------------------
forbes_country <-
  forbes_filter %>%
  group_by(country) %>%
  summarize(
    count = n(),
    max_net_worth = max(net_worth),
    min_net_worth = min(net_worth),
    dif_net_worth = max_net_worth - min_net_worth
  ) %>%
  filter(count >= 6) %>%
  arrange(dif_net_worth)

# Part 2 - Q5) ------------------------------------------------------------
ggplot(data = forbes_country) +
  geom_bar(
    mapping = aes(x = country, y = dif_net_worth),
    stat = "identity", fill = "coral3"
  ) +
  coord_flip() +
  xlab("Country") +
  ylab("Difference between the highest and lowest net worth in each country")

# Part 2 - Q6) ------------------------------------------------------------
ggplot(data = forbes_country) +
  geom_bar(
    mapping = aes(
      x = reorder(country, dif_net_worth),
      y = dif_net_worth
    ),
    stat = "identity", fill = "coral3"
  ) +
  geom_text(
    mapping = aes(x = country, y = dif_net_worth),
    label = forbes_country$dif_net_worth,
    hjust = -.1, size = 2
  ) +
  xlab("Country") +
  ylab("Difference between the highest and lowest net worth in each country") +
  coord_flip()

# Part 2 - Q7) ------------------------------------------------------------
forbes_rank <-
  forbes_filter %>%
  group_by(rank) %>%
  summarize(count = n()) %>%
  filter(count > 1)
{
  # As we can see, the data is first grouped by the ranks.
  # Then, we made a filter, keeping only those that have shared rankings.
  }
# Part 2 - Q8) ------------------------------------------------------------
forbes_mean_rank <- forbes_rank
forbes_mean_rank <-
  mutate(forbes_mean_rank, end_rank = rank + count - 1)
forbes_mean_rank <-
  mutate(forbes_mean_rank, mean_rank = ((rank + end_rank) / 2))
# By adding two columns, the mean of the ranks have been computed.

# Part 2 - Q9) ------------------------------------------------------------
forbes_country_worth <-
  forbes_filter %>%
  group_by(country) %>%
  summarize(total_net_worth = log(sum(net_worth)))

n <- joinCountryData2Map(forbes_country_worth,
                         joinCode = "NAME", nameJoinColumn = "country"
)
mapCountryData(n,
               nameColumnToPlot = "total_net_worth",
               mapTitle = "Net Worth of Forbes list of richest people by country",
               colourPalette = "terrain",
               addLegend = TRUE,
               oceanCol = "lightblue", missingCountryCol = "grey"
)