library(tidyverse)


# Question 1 ------------------------------------------------------------------------------------------------------


#' Tidy data for prefix
#'
#' @param data 
#' @param column_prefix 
#'
#' @return A tidy dataframe based on the designated criteria
#' @export 
#'
#' @examples 
tidy_df <- function(data, column_prefix = "var"){
  x <- str_subset(colnames(data), "^var")
  gather(data, x,
           key = "variable", value = "value")
}

# This is a function that I came up with. 
# I have designed a vector "x" that can read the column names from the dataset.
# Afterwards, it will match for what has been assigned in x. 
# Then, the gathering will work and it tries to tidy the data.
# As an example, bellow we can see how it works in "flights" dataset.
tidy_df <- function(data, column_prefix = "dep"){
  x <- str_subset(colnames(data), "^dep")
  gather(data, x,
         key = "variable", value = "value")
}
library(nycflights13)
example <- tidy_df(flights)



# Question 2 ------------------------------------------------------------------------------------------------------

#' Get the Jane Austen data
#'
#' It will attempt to install the right package for you. If it does not work,
#'   try to install it manually.
#'
#' @return A data frame with Jane Austen texts, one line per row
get_jane_austen_data <- function(){
  
  tryCatch({library(gutenbergr)}, error = function(e){install.packages("gutenbergr")})
  library(gutenbergr)
  
  austen_text <- gutenberg_works(author == "Austen, Jane") %>% 
    gutenberg_download(meta_fields = "title") %>% mutate(id = row_number(gutenberg_id))
  assign("austen_text", austen_text, envir=.GlobalEnv)
  invisible()
}
get_jane_austen_data()
View(austen_text)
#----------

extract_possible_names <- function(data3){
  data3 <- mutate(data3, a = sapply(str_extract_all(data3$text, 
                           '\\b[A-Z]\\w+'), paste, collapse = ' '))
}
example2 <- extract_possible_names(austen_text)
View(example2)

data1 <- austen_text
data1$a <- sapply(str_extract_all(data1$text, 
                                  '\\b[A-Z]\\w+'), paste, collapse = ' ')
data1$b <- str_count(data1$a, '\\w+')
max(data1$b)
x <- c(paste("var", 1:12, sep = ""))
data1 <- separate(data1, a, x, sep = " ", remove = TRUE)

tidy_df <- function(data, column_prefix = "var"){
  x <- str_subset(colnames(data), "^var")
  gather(data, x,
         key = "variable", value = "value")
}
data2 <- tidy_df(data1)
View(data2)

#----------
# extract_possible_names 
extract_possible_names <- function(data) {
  
}



# Question 3 ------------------------------------------------------------------------------------------------------

# filter_names



# Question 4 ------------------------------------------------------------------------------------------------------

# count_names_per_book
