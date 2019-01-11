library(tidyverse)
library(xml2)
library(RCurl)
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
get_population_ranking <- function(base_url){
  xpath_expressions <- c("country_link" = "//td[@class='region']/a/@href",
                         "country" = "//td[@class='region']/a",
                         "value" = "//tr/td[3]",
                         "rank" = "//tr/td[1]")
  url = str_c(base_url, "fields/335rank.html")
  #download url and execute all XPath queries which will each return a column for a data_frame
  raw_html <- read_html(getURL(url,
                               .encoding = "UTF-8",
                               .opts = list(followlocation = FALSE)))
  # I have used an iteration to put all the links as a list. 
  raw_list <- vector("list", length(xpath_expressions))
  for(i in seq_along(xpath_expressions)) {
    raw_list[[i]] <- xml_find_all(raw_html, xpath_expressions[i])
  }
  #make the necessary adjustments to the data frame as given by the assignment
  raw_data <- data.frame(country_link = sapply(raw_list[1], xml_text),
                         country = c(sapply(raw_list[2], xml_text)), 
                         population = c(sapply(raw_list[3], xml_text)),
                         rank.population = c(sapply(raw_list[4],xml_text)))
  raw_data$country_link <- lapply(raw_data$country_link, 
                                  gsub, pattern = "^\\W+", replacement = '')
  return(raw_data)
}
example1 <- get_population_ranking(base_url)

# Q1 - Testing without function -------------------------------------------
# In this section, I created the code and wanted to test whether it is working
#   outside the function, and then I inserted the code into the above function.
#xpath_expressions <- c("country_link" = "//td[@class='region']/a/@href",
#                       "country" = "//td[@class='region']/a",
#                       "value" = "//tr/td[3]",
#                       "rank" = "//tr/td[1]")
#url = str_c(base_url, "fields/335rank.html")

#raw_html <- read_html(getURL(url,
#                             .encoding = "UTF-8",
#                             .opts = list(followlocation = FALSE)))

#output <- vector("list", length(xpath_expressions))
#for(i in seq_along(xpath_expressions)) {
#  output[[i]] <- xml_find_all(raw_html, xpath_expressions[i])
#}
#output
#View(output)

#all_data <- data.frame(country_link = sapply(output[1], xml_text),
#                       country = c(sapply(output[2], xml_text)), 
#                       population = c(sapply(output[3], xml_text)),
#                       rank.population = c(sapply(output[4],xml_text)))
#all_data$country_link <- lapply(all_data$country_link, gsub, pattern='../', replacement='')
#View(all_data)

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
get_land_area <- function(data, country_link){
  xpath <- str_c("//div[@id='","field-area","']/div[",2,"]/span[2]")
  #download the file from country_link and execute the xpath query
  url2 = str_c(base_url, data$country_link)
  land_data <- c()
  for(i in 1:length(data$country_link)) {
     raw_html2 <- read_html(download_html(url2[i]))
#    raw_html2 <- read_html(getURL(url2[i],
#                                  .encoding = "UTF-8",
#                                  .opts = list(followlocation = FALSE)))
# Apparently, the speed of retrieving the data with the reading function used is
#  better than then one bellow. Thay is why I have changed it.     
    raw_list_land <- xml_find_all(raw_html2, xpath)
    land_data[i] <- xml_text(raw_list_land)
  }
  return(land_data)
}
example2 <- get_land_area(example1, "country_link")

# Q2 - Testing without function -------------------------------------------
#xpath <- str_c("//div[@id='","field-area","']/div[",2,"]/span[2]")
#url2 = str_c(base_url, "geos/ch.html")
#raw_html2 <- read_html(getURL(url2,
#                             .encoding = "UTF-8",
#                             .opts = list(followlocation = FALSE)))
#land_area <- xml_find_all(raw_html2, xpath)
#output2 <- xml_text(land_area)
#output2

#xpath <- str_c("//div[@id='","field-area","']/div[",2,"]/span[2]")
#url2 = str_c(base_url, example1$country_link)
#raw_list_land <- vector("list", length(example1$country_link))
#land_data <- c()
#for(i in 1:3) {
#  raw_html2 <- read_html(getURL(url2[i],
#                                .encoding = "UTF-8",
#                                .opts = list(followlocation = FALSE)))
#  raw_list_land <- xml_find_all(raw_html2, xpath)
#  land_data[i] <- xml_text(raw_list_land)
#}
#View(land_data)

#for(i in seq_along(example1)) {
#  raw_list_land[[i]] <- xml_find_all(raw_html2, xpath)
#  area <- xml_text(raw_list_land)
#}
#land_area <- xml_find_all(raw_html, xpath)
#output2 <- xml_text(land_area)

# Q3 - Answer -------------------------------------------------------------
#' Question 3: Get Population Density
#'
#' @return
#' @export
#'
#' @examples
get_population_density <- function(){
  
}


#' Question 4: Get All Provided Rankings
#'
#' @return
#' @export
#'
#' @examples
get_rankings <- function(){
  url <- "https://www.cia.gov/library/publications/the-world-factbook/docs/rankorderguide.html"
  xpath <- c("characteristic" = "//div[@class='field_label']/strong/a",
             "characteristic_link" = "//div[@class='field_label']/strong/a/@href")
  #...
}


#' Question 5 - Part 1: Get Ranking
#'
#' @param url The url of the ranking
#' @param characteristic What this ranking is about
#'
#' @return
#' @export
#'
#' @examples
get_ranking <- function(url = "fields/335rank.html", characteristic = "population"){
  xpath_expressions <- c("country_link" = "//td[@class='region']/a/@href",
                         "country" = "//td[@class='region']/a",
                         "value" = "//tr/td[3]",
                         "rank" = "//tr/td[1]")
  #...
}

#' Question 5 - Part 2: Get Country Characteristic
#'
#' @param country_link 
#' @param xpath_field_id 
#' @param item 
#'
#' @return
#' @export
#'
#' @examples
get_country_characteristic <- function(country_link, xpath_field_id = "field-area", item = 2){
  #update the xpath and use similar code other than that
}


#' Question 6: Combine Rankings
#'
#' @param rankings Rankings from get_rankings (or a selection thereof)
#'
#' @return
#' @export
#'
#' @examples
combine_rankings <- function(rankings){
  
}



