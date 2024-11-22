#' @noRd
#' @keywords internal
hyper_mutate_cols <- function(.data) {
  .data
}

# mtcars_tibble <- tibble::tibble(mtcars)
# microbenchmark::microbenchmark(
#   tibble_obj = dplyr::mutate(mtcars_tibble, new_col = mpg * 1e9),
#   dataframe_obj = dplyr::mutate(mtcars, new_col = mpg * 1e9),
#   times=10L
# )
