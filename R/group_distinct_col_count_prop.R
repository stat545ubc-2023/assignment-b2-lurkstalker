#'
#' @title Group by a Column and Calculate Proportion of Distinct Values in Another Column
#' @description
#' This function groups a given dataset by a specified column and then calculates
#' the proportion of distinct values in another specified column within each
#' group. It is designed to provide a quick and generic way to summarize data,
#' which is useful in exploratory data analysis.
#' @param dataset A data frame containing the data to be grouped and summarized.
#' @param group_col A string representing the name of the column to group by.
#' @param summary_col A string representing the name of the column to calculate
#' the proportion of distinct values.
#' @return: A data frame containing the grouping column, the total count of
#' distinct values in the summary_col for each group, and the proportion of
#' these distinct values relative to the total distinct values in
#' summary_col across the entire dataset.
#' @examples
#' library(dplyr)
#'
#' # Example data frame
#' test_data <- data.frame(
#'   Species = c("Oak", "Maple", "Oak", "Pine", "Maple", "Pine", NA),
#'   Height = c(10, 15, 20, 25, 30, 35, 40)
#' )
#'
#' # Usage of the group_distinct_col_count_prop function for with no NA's
#' result1 <- group_distinct_col_count_prop(test_data[1:6, ], "Species", "Height")
#'
#' # Usage of the group_distinct_col_count_prop function for with NA's
#' result2 <- group_distinct_col_count_prop(test_data, "Species", "Height")
#'
#' @export

group_distinct_col_count_prop <- function(dataset, group_col, summary_col) {
  stopifnot(is.data.frame(dataset))

  # Check if group_col and summary_col are present in the dataset
  if (!(group_col %in% colnames(dataset))) {
    stop("Error: group_col is not a column in the dataset.")
  }
  if (!(summary_col %in% colnames(dataset))) {
    stop("Error: summary_col is not a column in the dataset.")
  }

  total_summary_col_distinct_count <- dplyr::n_distinct(dataset[[summary_col]], na.rm = TRUE)

  result <- dataset %>%
    dplyr::group_by(.data[[group_col]]) %>%
    dplyr::summarise(total = dplyr::n_distinct(.data[[summary_col]], na.rm = TRUE)) %>%
    dplyr::mutate(proportion = total / total_summary_col_distinct_count)
  return(result)
}
