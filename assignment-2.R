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
tidy_df <- function(data, column_prefix = "var") {
  x <- str_subset(colnames(data), "^var")
  gather(data, x, key = "variable", value = "value")
}

# This is a function that I came up with.
# I have designed a vector "x" that can read the column names from the dataset.
# Afterwards, it will match for what has been assigned in x.
# Then, the gathering will work and it tries to tidy the data.
# As an example, bellow we can see how it works in "flights" dataset.
tidy_df <- function(data, column_prefix = "dep") {
  x <- str_subset(colnames(data), "^dep")
  gather(data, x,
    key = "variable", value = "value"
  )
}
library(nycflights13)
example <- tidy_df(flights)
View(flights)
View(example)

# Question 2 ------------------------------------------------------------------------------------------------------

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
#' "text_id" contains the original id from the dataframe.
#' "name" contains each capwords that was extracted from each row of dataframe.
#' "id" contains a unique id for every capwords.
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
  extracted_data <- mutate(extracted_data,
    numberof_cap_words =
      str_count(extracted_data$cap_words, "\\w+")
  )
  max_cap_words <- max(extracted_data$numberof_cap_words)
  x <- c(paste("var", 1:max_cap_words, sep = ""))
  extracted_data <- separate(extracted_data, cap_words, x,
    sep = " ",
    remove = TRUE
  )
  #  extracted_data <- rename(extracted_data, text_id = id_col, name = value)
  #  changed_data <- tidy_df(extracted_data)
  extracted_data <- mutate_if(extracted_data, is_character, funs(na_if(., "")))
  changed_data <- tidy_df(extracted_data)
  changed_data <- rename(changed_data, text_id = id_col, name = value)
  changed_data <- mutate(changed_data, id = rownames(changed_data))
  finished_data <- select(changed_data, text_id, name, id)
  finished_data[!is.na(finished_data$name), ]
}
austen_cap_words <- extract_possible_names(austen_text, "text", "id")
View(austen_cap_words)


# Question 3 ------------------------------------------------------------------------------------------------------
austen_word_freqs <- readRDS("austen_word_freqs.Rds")

# filter_names
#' Title
#'
#' @param data The output from previous question namely "austen_cap_words"
#' @param reference The reference database which contains the count of words.
#' @param word_col The columns by which the whole comparison, joining, and
#'                 tidying will go into effect. 
#'
#' @return
#' @export
#'
#' @examples
filter_names <- function(data, reference, word_col) {
  changed_data <- data
  changed_data$lower_words <- tolower(data[[word_col]])
  summarized_data <- changed_data %>%
    group_by(lower_words) %>%
    summarise(numberof_times_repeated = n()) %>%
    rename(word = lower_words) %>%
    inner_join(reference, by = "word") %>%
    mutate(percentage = (numberof_times_repeated / count) * 100)

  changed_data %>%
    rename(word = lower_words) %>%
    inner_join(summarized_data, by = "word") %>%
    filter(percentage >= 75) %>%
# I have noticed that there are particular addressing pronouns in the data.
# To have the correct list of Names, I thought that I should omit these.
    filter(word != "sir") %>% 
    filter(word != "mrs") %>% 
    filter(word != "miss") %>% 
    filter(word != "mr")
}

filtered_words <- filter_names(austen_cap_words, austen_word_freqs, "name")
View(filtered_words)


# Question 4 ------------------------------------------------------------------------------------------------------

# count_names_per_book
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

