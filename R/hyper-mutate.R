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
#' df <- data.frame(a = sample(1:100, 100), b = sample(1000:2000, 100))
#' new_df <- hyper_mutate(df, c = a*b, d = c+a)
#'
#' # Show results
#' tibble::tibble(new_df)
#'
#' \dontrun{
#'
#' # benchmarking example:
#' df <- data.frame(a = sample(1:1e7, 1e7), b = sample(1:1e7, 1e7), c = sample(1:1e7, 1e7), d = sample(1:1e7, 1e7))
#'
#' microbenchmark::microbenchmark(
#'   hyperdplyr = hyper_mutate(df, e = a*b, f = c+a, g=exp(f)),
#'   dplyr = dplyr::mutate(df, e = a*b, f = c+a, g=exp(f))
#'   )
#' }
#'
#' @return data.frame
#' @export
hyper_mutate <- function(.data, ...) {
  UseMethod("hyper_mutate")
}

#' Hyper Mutate implementation for Data Frames
#'
#' Preferred over tibbles as tibble transformations can be slower than
#' data.frame. Can be compared using:
#'
#' @param .data
#' @param ...
#'
#' @export
hyper_mutate.data.frame <- function(.data, ...) {
  return(mutate_class_wrapper(df, c = a*b, d = c+a))
}

#' Mutate Data Frame R6 Class wrapper
#'
#' Class wrapper for `MutateDataFrame` R6 class
#'
#' @param .data
#' @param ...
#'
#' @export
mutate_class_wrapper <- function(.data, ...) {
  data_obj <- MutateDataFrame$new(.data)
  data_obj$mutate_class(...)

  return(data_obj$data_)
}

#' Mutate Data Frame R6 Class implementation
#'
#' R6 class for modifying/mutating data frames
#'
#' @param .data
#' @param ...
#' @import rlang
#' @import R6
#' @export
MutateDataFrame <- R6Class("MutateDataFrame",
   public = list(
     data_ = NULL,
     initialize = function(data_) {
       self$data_ <- data_
     },
     mutate_class = function(...) {
       dots <- enexprs(...)

       # loops through names and evaluates lazy expressions
       for (name in names(dots)) {
         self$data_[[name]] <- eval(
           dots[[name]],
           envir = self$data_,
           enclos = parent.frame()
         )
       }
     }
   )
)
