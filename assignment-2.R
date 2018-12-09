#' Title: Assignment 2 - R Programming Course
#' Author: Mohamadreza Hoseinpour

# Loading Libraries ------------------------------------------------------------
library(tidyverse)

# Question 1 -------------------------------------------------------------------

#' Tidy data for prefix
#'
#' @param data The dataframe as the input.
#' @param column_prefix the column based on which the tidying will happen.
#'
#' @return A tidy dataframe based on the designated criteria
#' @export
#'
#' @examples 
tidy_df <- function(data, column_prefix = "var") {
  x <- str_subset(colnames(data), "^var")
  gather(data, x, key = "variable", value = "value")
}

#' This is a function that I came up with.
#'   I have designed a vector "x" that can read the column names from the dataset.
#'   Afterwards, it will match for what has been assigned in x.
#'   Then, the gathering will work and it tries to tidy the data.
#'   As an example, bellow we can see how it works in "flights" dataset.
#'   
#' tidy_df <- function(data, column_prefix = "dep") {
#'  x <- str_subset(colnames(data), "^dep")
#'  gather(data, x, key = "variable", value = "value")
#' }
#' library(nycflights13)
#' example <- tidy_df(flights)
#' View(flights)
#' View(example)

# Question 2 -------------------------------------------------------------------

#' Get the Jane Austen data
#'
#' It will attempt to install the right package for you. If it does not work,
#'   try to install it manually.
#'
#' @return A data frame with Jane Austen texts, one line per row
get_jane_austen_data <- function() {
  tryCatch({
    library(gutenbergr)
  }, error = function(e) {
    install.packages("gutenbergr")
  })
  library(gutenbergr)

  austen_text <- gutenberg_works(author == "Austen, Jane") %>%
    gutenberg_download(meta_fields = "title") %>%
    mutate(id = row_number(gutenberg_id))
  assign("austen_text", austen_text, envir = .GlobalEnv)
  invisible()
}
get_jane_austen_data()
View(austen_text)

#----------

#' Extract capital words
#'
#' @param data the dataframe as the input
#' @param text_col the column which contains the text
#' @param id_col the column which contains the initial id in the dataframe
#'
#' @return A table with three columns "text_id, name, id"
#'   "text_id" contains the original id from the dataframe.
#'   "name" contains each capwords that was extracted from each row of dataframe.
#'   "id" contains a unique id for every capwords.
#' @export
#'
#' @examples
extract_possible_names <- function(data, text_col, id_col) {
  extracted_data <- data
  extracted_data <- mutate(extracted_data,
    cap_words =
      sapply(str_extract_all(
        extracted_data[[text_col]],
        "\\b[A-Z]\\w+"
      ), paste, collapse = " ")
  )
#' This code is used to extract the capital words from the text.

  extracted_data <- mutate(extracted_data,
    numberof_cap_words =
      str_count(extracted_data$cap_words, "\\w+")
  )

#' This code is used to count the number of capital word. Afterwards, 
#'   the maximum number of words will be used to come up with proper number of 
#'   columns. Hence, the capital words can be separated into separate columns.
  max_cap_words <- max(extracted_data$numberof_cap_words)
  x <- c(paste("var", 1:max_cap_words, sep = ""))
  extracted_data <- separate(extracted_data, cap_words, x,
    sep = " ",
    remove = TRUE
  )
  
#' From here forward, the "tidy_df" function will be used to tidy the data.
#'   Also, a rename of the id will take into effect and a handful of columns
#'   will be selected. At the end, all rows which contain "NA"s in all columns
#'   of variables, indicating the existence of capital words, will be removed.

  extracted_data <- mutate_if(extracted_data, is_character, funs(na_if(., "")))
#' I have used this special line of code to convert "NULL" rows in the first
#'   column "var1" into "NA"s so that I can omit the "NA"s afterward.

  changed_data <- tidy_df(extracted_data)
  changed_data <- rename(changed_data, text_id = id_col, name = value)
  changed_data <- mutate(changed_data, id = rownames(changed_data))
  finished_data <- select(changed_data, text_id, name, id)
  finished_data[!is.na(finished_data$name), ]
}
#' For some reasons, piping was not working when I was designing this code. 
#'   Hence, I had to go forward with assignment of variables to have this 
#'   function working, taking into consideration all of its components and 
#'   the way of analyzing and tidying the data.
austen_cap_words <- extract_possible_names(austen_text, "text", "id")
View(austen_cap_words)


# Question 3 -------------------------------------------------------------------
austen_word_freqs <- readRDS("austen_word_freqs.Rds")
#' This code is used to read the data provided in the assignment, as the 
#'   reference for tidying the data. 

# filter_names
#'
#' @param data The output from previous question namely "austen_cap_words"
#' @param reference The reference database which contains the count of words.
#' @param word_col The columns by which the whole comparison, joining, and
#'                 tidying will go into effect.
#'
#' @return A comprehensive table containing "text_id" which is the original id 
#'         from the data in previous question. "name" is the column which 
#'         contains the capital words. "id" is the unique id to each capital
#'         word. "word" is the column containing the word from the reference 
#'         dataset. The other three columns contain the numbers of each name 
#'         repeated and the calculation pertaining to the percentage of each 
#'         word's apperance in the original data.
#' @export
#'
#' @examples
filter_names <- function(data, reference, word_col) {
  changed_data <- data
  changed_data$lower_words <- tolower(data[[word_col]])
#' I had to convert the words into lowercase so that I can compare the words to
#'   those in the reference dataset, and then make a new datafram by joining the
#'   datasets. Then the rest of the computation take place. For this to happen,
#'   a grouping and summarizing will happen.
  summarized_data <- changed_data %>%
    group_by(lower_words) %>%
    summarize(numberof_times_repeated = n()) %>%
    rename(word = lower_words) %>%
    inner_join(reference, by = "word") %>%
    mutate(percentage = (numberof_times_repeated / count) * 100)

  changed_data %>%
    rename(word = lower_words) %>%
    inner_join(summarized_data, by = "word") %>%
    filter(percentage >= 75) %>%
#' I have noticed that there are particular addressing pronouns in the data.
#'   To have the correct list of Names, I thought that I should omit these. To 
#'   do this, I used lowercase words from the reference dataframe.
    filter(word != "sir") %>%
    filter(word != "mrs") %>%
    filter(word != "miss") %>%
    filter(word != "mr")
}
filtered_words <- filter_names(austen_cap_words, austen_word_freqs, "name")
View(filtered_words)

# Question 4 -------------------------------------------------------------------

# count_names_per_book
#'
#' @param data The original data.
#' @param data_counts The data from the previous question which contains the 
#'                    calculation for the percentage of words.
#'
#' @return A table with three columns namely "title, unique_names, 
#'         name_occurences". The "title" contains the title of the book by the
#'         author. The "unique_name" contains the number of unique names per 
#'         book. The "name_occurences" contains the total number of name
#'         occurrences per book.
#' @export
#'
#' @examples
count_names_per_book <- function(data, data_counts) {
  extracted_data <- data %>%
    select(title, id) %>%
    rename(text_id = id)

  joined_data <- inner_join(extracted_data, filtered_words, by = "text_id") %>%
    select(title, text_id, name, id)

  names_per_book <- joined_data %>%
    group_by(title) %>%
    summarize(unique_names = length(unique(name)), name_occurrences = n())
}

filtered_names <- count_names_per_book(austen_text, filtered_words)
View(filtered_names)
#' Answer to questions in Q4:
#' We can see from this table that the book "The Complete Project Gutenberg 
#'   Works of Jane Austen A Linked Index of all PG Editions of Jane Austen"
#'   contains the highest number of unique names.
#'   
#' We can also see that the same book has the most occurences of names. 