# Define the function
file_path <- "./inst/example-data/bulletin_monthly/lookup_with_cond_test.csv"
key <- "test"
language <- "G"

get_text <- function(file_path, key, language) {
  # Read the CSV file
  data <- read.csv(file_path, sep = ";", stringsAsFactors = FALSE)
  
  # Subset the row for the given key
  row <- data[data$key == key, ]
  
  # Check if the key exists in the data
  if (nrow(row) == 0) {
    stop("Key not found in the data.")
  }
  
  # Evaluate the condition
  condition_met <- eval(parse(text = row$condition))
  
  # Return the text for the specified language if the condition is met
  if (condition_met) {
    return(glue::glue(row[[language]]))
  } else {
    return("")
  }
}
