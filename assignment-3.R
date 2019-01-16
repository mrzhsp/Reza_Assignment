#' Title: Assignment 3 - R Programming Course
#' Author: Mohamadreza Hoseinpour

# Loading Libraries ------------------------------------------------------------
library(tidyverse)
library(xml2)
library(RCurl)
library(dplyr)
base_url <- "https://www.cia.gov/library/publications/the-world-factbook/"

# Q1 - Answer -------------------------------------------------------------
#' Question 1: Get Population Ranking
#'
#' @param base_url The URL as the input.
#'
#' @return A tidy dataframe including 4 columns.
#'   "country_link" contains the link to each country in the webpage.
#'   "country" contains the name of the respective country.
#'   "population" contains the population of each country.
#'   "rank.population" contains the respective rankd of each country based on
#'     population.
#' @export
#'
#' @examples
get_population_ranking <- function() {
  xpath_expressions <- c(
    "country_link" = "//td[@class='region']/a/@href",
    "country" = "//td[@class='region']/a",
    "value" = "//tr/td[3]",
    "rank" = "//tr/td[1]"
  )
  url1 <- str_c(base_url, "fields/335rank.html")
  # download url and execute all XPath queries which will each return a column for a data_frame
  raw_html <- read_html(getURL(url1,
    .encoding = "UTF-8",
    .opts = list(followlocation = FALSE)
  ))
  # I have used an iteration to put all the links as a list.
  raw_list <- vector("list", length(xpath_expressions))
  for (i in seq_along(xpath_expressions)) {
    raw_list[[i]] <- xml_find_all(raw_html, xpath_expressions[i])
  }
  # make the necessary adjustments to the data frame as given by the assignment
  raw_data <- data.frame(
    country_link = sapply(raw_list[1], xml_text),
    country = c(sapply(raw_list[2], xml_text)),
    population = c(sapply(raw_list[3], xml_text)),
    rank.population = c(sapply(raw_list[4], xml_text))
  )
  raw_data$country_link <- lapply(raw_data$country_link,
    gsub,
    pattern = "^\\W+", replacement = ""
  )
  return(raw_data)
}
country_pop <- get_population_ranking()

# Q2 - Answer -------------------------------------------------------------
#' Question 2: Retrieve Land Area
#'
#' @param country_link A character vector of one or more country_link urls
#' @param data The dataset that is used as the input
#'
#' @return A character vector
#'
#' @export
#'
#' @examples
get_land_area <- function(data, country_link) {
  xpath <- str_c("//div[@id='", "field-area", "']/div[", 2, "]/span[2]")
  # download the file from country_link and execute the xpath query
  url2 <- str_c(base_url, data$country_link)
  land_data <- c()
  for (i in 1:length(data$country_link)) {
    raw_html2 <- read_html(download_html(url2[i]))
    #    raw_html2 <- read_html(getURL(url2[i],
    #                                  .encoding = "UTF-8",
    #                                  .opts = list(followlocation = FALSE)))
    # Apparently, the speed of retrieving the data with the reading function used is
    #  better than then one above. Thay is why I have changed it to
    #  read_html(download_html)
    raw_list_land <- xml_find_all(raw_html2, xpath)
    land_data[i] <- xml_text(raw_list_land)
  }
  return(land_data)
}
land_area <- get_land_area(country_pop, "country_link")

# Q3 - Answer -------------------------------------------------------------
#' Question 3: Get Population Density
#'
#' @return A tidy dataframe including 4 columns from the first function. and
#'   "land_area" including the land of each country in sq km.
#'   "population_density" including the density of each country.
#' @export
#'
#' @examples
get_population_density <- function() {
  country_pop_land <- cbind(country_pop, land_area)
  country_pop_land$land_area <- parse_number(country_pop_land$land_area)
  country_pop_land[12, "land_area"] <- 1000000
  country_pop_land$population <- parse_number(country_pop_land$population)
  country_pop_land <- mutate(country_pop_land,
    population_density = population / land_area
  )
  View(country_pop_land)
  return(country_pop_land)
}
country_pop_land <- get_population_density()

# Q4 - Answer -------------------------------------------------------------
#' Question 4: Get All Provided Rankings
#'
#' @return A tidy dataframe including 2 columns.
#'   "characteristic" including all the characteristics for all countries.
#'   "characteristic_link" including the additional link to the respective page.
#' @export
#'
#' @examples
get_rankings <- function() {
  url4 <- "https://www.cia.gov/library/publications/the-world-factbook/docs/rankorderguide.html"
  xpath <- c(
    "characteristic" = "//div[@class='field_label']/strong/a",
    "characteristic_link" = "//div[@class='field_label']/strong/a/@href"
  )
  ranking_html <- read_html(getURL(url4,
    .encoding = "UTF-8",
    .opts = list(followlocation = FALSE)
  ))
  ranking_list <- vector("list", length(xpath))
  for (i in seq_along(xpath)) {
    ranking_list[[i]] <- xml_find_all(ranking_html, xpath[i])
  }
  ranking_data <- data.frame(
    characteristic = sapply(ranking_list[1], xml_text),
    characteristic_link = c(sapply(ranking_list[2], xml_text))
  )
  ranking_data$characteristic_link <- lapply(ranking_data$characteristic_link,
    gsub,
    pattern = "^\\W+", replacement = ""
  )
  ranking_data$characteristic <- lapply(ranking_data$characteristic,
    gsub,
    pattern = "\\:$", replacement = ""
  )
  ranking_data$characteristic <- tolower(ranking_data$characteristic)
  View(ranking_data)
  return(ranking_data)
}
ranking_data <- get_rankings()

# Q5.a - Answer -------------------------------------------------------------
#' Question 5 - Part 1: Get Ranking
#'
#' @param url The url of the ranking
#' @param characteristic What this ranking is about
#'
#' @return A tidy dataframe including 4 columns.
#'   "country_link" contains the link to each country in the webpage.
#'   "country" contains the name of the respective country.
#'   "population" contains the characteristic of each country which I have
#'     identified in the input for the function.
#'   "rank.population" contains the respective rankd of each country based on
#'     population.
#' @export
#'
#' @examples
get_ranking <- function(url, char) {
  xpath_expressions <- c(
    "country_link" = "//td[@class='region']/a/@href",
    "country" = "//td[@class='region']/a",
    "value" = "//tr/td[3]",
    "rank" = "//tr/td[1]"
  )
  url51 <- str_c(base_url, url)
  raw_html <- read_html(getURL(url51,
    .encoding = "UTF-8",
    .opts = list(followlocation = FALSE)
  ))
  raw_list <- vector("list", length(xpath_expressions))
  for (i in seq_along(xpath_expressions)) {
    raw_list[[i]] <- xml_find_all(raw_html, xpath_expressions[i])
  }
  country_ranking <- data.frame(
    country_link = sapply(raw_list[1], xml_text),
    country = c(sapply(raw_list[2], xml_text)),
    characteristic = c(sapply(raw_list[3], xml_text)),
    rank = c(sapply(raw_list[4], xml_text))
  )
  country_ranking$country_link <- lapply(country_ranking$country_link,
    gsub,
    pattern = "^\\W+", replacement = ""
  )
  country_ranking <- rename(country_ranking, !!char := characteristic)
  colnames(country_ranking)[4] <- paste("rank", char, sep = ".")
  #  country_ranking <- rename(country_ranking, !!rank.char := rank)
  #  View(country_ranking)
  return(country_ranking)
}
country_ranking <- get_ranking("fields/343rank.html", "median age")
# I have desgined the function slightly different, in the way that I give
#  inputs. In this way, I have tested that if I use other link such as
#  "fields/343rank.html" and "median age" as another characteristic, the ranking
#  table will work correctly.
# In addition, I used "char" as the input for character to avoid mix up.

# Q5.b - Answer -----------------------------------------------------------
#' Question 5 - Part 2: Get Country Characteristic
#'
#' @param country_link is the dataframe from which the links from countries will
#'   be read. In this case, the output from the previous function, namely 5.a
#'   is used as the input.
#' @param xpath_field_id is the characteristic that we want to see in our output.
#'   In this case we use the "field-area"
#' @param item is the item that is used in each characteristic for every
#'   country.
#'
#' @return A character vector of all countries' characteristic.
#' @export
#'
#' @examples
get_country_characteristic <- function(country_link, xpath_field_id, item) {
  # update the xpath and use similar code other than that
  xpath <- str_c("//div[@id='", xpath_field_id, "']/div[", item, "]/span[2]")
  # download the file from country_link and execute the xpath query
  url52 <- str_c(base_url, country_link$country_link)
  char_data <- c()
  for (i in 1:length(country_link$country_link)) {
    raw_html5 <- read_html(download_html(url52[i]))
    #    raw_html5 <- read_html(getURL(url52[i],
    #                                  .encoding = "UTF-8",
    #                                  .opts = list(followlocation = FALSE)))
    # Apparently, the speed of retrieving the data with the reading function used is
    #  better than then one above. Thay is why I have changed it to
    #  read_html(download_html)
    raw_list_land <- xml_find_all(raw_html5, xpath)
    char_data[i] <- xml_text(raw_list_land)
  }
  return(char_data)
}
country_char <- get_country_characteristic(country_ranking, "field-area", 2)
# As an example, I used "item = 1" to see whether the data will be read
#  correctly and it was the case indeed. When I changed the item to 1, the
#  "total area" will be read which is the correct answer.

# Q6 - Answer -------------------------------------------------------------
#' Question 6: Combine Rankings
#'
#' @param rankings Rankings from get_rankings (or a selection thereof)
#'
#' @return A tidy dataframe including 156 columns.
#'   These columns are comprised of the link, country, multiple characteristics
#'   and the respective rank of that country in each characteristics.
#' @export
#'
#' @examples
combine_rankings <- function(rankings) {
  raw_ranking <- c()
  total_rankings <- get_ranking(
    rankings[1, "characteristic_link"],
    rankings[1, "characteristic"]
  )

  for (i in 2:nrow(rankings)) {
    url6 <- rankings[i, "characteristic_link"]
    char6 <- rankings[i, "characteristic"]
    new_rankings <- get_ranking(url6, char6)
    new_rankings <- new_rankings[, (-1)]
    total_rankings <- full_join(total_rankings, new_rankings, by = "country")
  }
  return(total_rankings)
}
final_data <- combine_rankings(ranking_data)
View(final_data)

# The End.