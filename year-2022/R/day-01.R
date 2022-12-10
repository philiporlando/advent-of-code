# Read data
raw <- readr::read_lines("year-2022/data/day-01.txt")

# Define helper to parse elf inventories
parse_elf_inventories <- function(raw) {

  # Define empty nested list to append to
  # I had to add NA values because I didn't know how to append to the end of an empty list
  # The initial NA value prevents length(out) from returning 0 during the first iteration
  out <- list(c(NA))

  # Loop through each line in the input file
  for (val in raw) {

    # Create a new nested list with NA value when the next elf inventory is detected
    if (val == "") {
      # Not possible to append an empty list...
      out <- append(out, c(NA))
    } else {
      # Otherwise, append the current calorie value to the existing elf's nested list
      n <- length(out)
      out[[n]] <- c(out[[n]], as.numeric(val)) # this syntax hurts my soul
    }

  }

  return(out)
}

# Define helper to summarize the top n elf inventories
sum_calories <- function(inventories, top_n = 1) {
  sums <- purrr::map_dbl(inventories, ~sum(., na.rm = TRUE)) %>%
    sort(decreasing = TRUE)
  out <- sum(sums[1:top_n])
}

# Define main function
solve <- function(raw, top_n) {
  inventories <- parse_elf_inventories(raw)
  sums <- sum_calories(inventories, top_n)
  return(sums)
}

# Apply solution to some test data
assertthat::are_equal(solve(c(1, 2, "", 3, 4, "", 5, 6), top_n = 1), 11)
assertthat::are_equal(solve(c(7, 8, "", 9, 10, "", 11, 12), top_n = 3), 57)

# Return the solution
glue::glue("Solution to part 1: {solve(raw, 1)}")
glue::glue("Solution to part 2: {solve(raw, 3)}")
