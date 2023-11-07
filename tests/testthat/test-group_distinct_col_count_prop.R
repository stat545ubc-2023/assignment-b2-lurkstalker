library(dplyr)
# Define the function group_distinct_col_count_prop here

# Sample test data
test_data <- data.frame(
  Species = c("Oak", "Maple", "Oak", "Pine", "Maple", "Pine", NA),
  Height = c(10, 15, 20, 25, 30, 35, 40)
)

# Test cases
test_that("group_distinct_col_count_prop function works correctly", {

  # Test 1: Vector with no NA's
  result1 <- group_distinct_col_count_prop(test_data[1:6, ], "Species", "Height")
  result1
  expect_equal(nrow(result1), 3)
  expect_equal(sum(result1$proportion), 1)

  # Test 2: Vector that has NA's
  result2 <- group_distinct_col_count_prop(test_data, "Species", "Height")
  expect_equal(nrow(result2), 4)
  expect_equal(sum(result2$proportion, na.rm = TRUE), 1)

  # Test 3: Vector of length 0
  empty_data <- test_data[FALSE, ]
  result3 <- group_distinct_col_count_prop(empty_data, "Species", "Height")
  expect_equal(nrow(result3), 0)
})

# Test 4: Vector that hasn't the column specified in the function arguments
test_that("group_distinct_col_count_prop handles non-existent columns correctly", {
  expect_error(group_distinct_col_count_prop(test_data, "Species", "Heights"),
               "Error: summary_col is not a column in the dataset.")
})
