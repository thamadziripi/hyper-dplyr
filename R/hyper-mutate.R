#' An efficient alternative approach to `dplyr` mutate function
#'
#' `hyper_mutate`, similiar to `dplyr::mutate`, is a lightweight and highly
#'   efficient data manipulation function, that addresses four main
#'   limitations found in dplyr, namely data copying, evaluation context
#'   overhead, type conversion and its reliance on tidyverse.
#'
#' @section `dplyr::mutate` performance limitations:
#'
#' [1] Data copying: `dplyr::mutate` creates a new data frame when
#'   modifying or adding columns rather than modifying the data in place.
#'   Instead, `hyper_mutate` prioritises performance by modifying the data in
#'   place removing any memory overhead.
#'
#' [2] Evaluation context: `dplyr::mutate` uses non-standard evaluation to
#'   evaluate transformations. For example, column names being used directly
#'   without quotes requires parsing and environment specific evaluation.
#'   Although it makes `dplyr::mutate` user friendly, its inherently slower
#'   than direct evaluation.
#'
#' [3] Type conversion: When adding new columns, `dplyr::mutate` checks and
#'   ensures type consistency across operations. This can slow down operations
#'   involving multiple transformations, as every operation may require checks
#'   and adjustments.
#'
#' [4] Tidyverse: `dplyr::mutate` is tightly integrated with a number of
#'   `tidyverse` constructs. These abstractions can introduce overhead compared
#'   to raw base R operations, especially for small, tight loops or highly
#'   specific transformations.
#'
#' @param .data A data frame, data frame extension (e.g. a tibble), or a
#'   lazy data frame (e.g. from dbplyr or dtplyr).
#' @param ... <[`data-masking`][rlang::args_data_masking]> Name-value pairs.
#'   The name gives the name of the column in the output.
#'
#' @examples
#' @return
#' @export
hyper_mutate <- function(.data, ...) {
  UseMethod("hyper_mutate")
}

#' @rdname hyper_mutate
#'
#' Preferred over tibbles as tibble transformations can be slower than
#' data.frame. Can be compared using:
#'
#' @param .data
#' @param ...
#'
#' @examples
#' @return
#' @export
hyper_mutate.data.frame <- function(.data, ...) {
  cat("shape of data.frame: ", dim(.data))

  dots <- list(...)

  hyper_mutate_cols(data)
}

#' @rdname hyper_mutate
#'
#' @param .data
#' @param ...
#'
#' @examples
#' @return
#' @export
hyper_mutate.vector <- function(.data, ...) {
  cat("size of vector: ", length(.data))

}

