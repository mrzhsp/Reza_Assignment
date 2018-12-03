library(nycflights13)
data(flights)
View(flights)


tidy_df <- function(data, column_prefix = "dep"){
  x <- str_subset(colnames(data), "^dep")
  gather(data, x,
         key = "variable", value = "value")
}
example <- tidy_df(flights)

new_flights <- gather(flights, 
                      x,
                      key = "variable", value = "value")

unique(new_flights$variable)
x
remove(example)

