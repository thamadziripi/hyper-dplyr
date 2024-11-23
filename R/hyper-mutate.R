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
#'
#' df <- data.frame(a = sample(1:100, 100), b = sample(1000:2000, 100))
#' new_df <- hyper_mutate(df, c = a*b, d = c+a)
#'
#' @return data.frame
#' @export
hyper_mutate <- function(.data, ...) {
  UseMethod("hyper_mutate")
}

#' Hyper Mutate implementation for Data Frames
#'
#' Hyper-mutate relies on a R6 wrapper which imitates the primary behavior of
#' `dplyr::mutate`. It's very light weight by design and prioritizes
#' performance by reducing R's copy-on-modify behavior. R6 classes by nature
#' allow for in-place modifications, reducing the overhead of having to
#' frequently copy objects to memory.
#'
#' @inheritParams hyper_mutate
#' @export
hyper_mutate.data.frame <- function(.data, ...) {
  mutate_class_wrapper(.data, ...)
}

#' Mutate Data Frame R6 Class wrapper
#'
#' Class wrapper for `MutateDataFrame` R6 class
#'
#' @inheritParams hyper_mutate
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
#' @import rlang
#' @import R6
#' @export
MutateDataFrame <- R6Class("MutateDataFrame",
   public = list(
     #' @field data_ data.frame to be modified
     data_ = NULL,

     #' @description Creates a new data.frame object
     #'
     #' @param data_ data.frame
     initialize = function(data_) {
       self$data_ <- data_
     },

     #' @description Modifies data_ by either changing an existing column or
     #' adding a new column. Modifications are determined by (...), which
     #' should represent a named expression to be evaluated. `mutate_class`
     #' interfaces the same way as `dplyr::mutate`.
     #'
     #' @param ... name-value pairs where the name is the column to be modified
     #' or added. Value is the expression to be evaluated.
     #'
     #' @examples
     #' \dontrun{data_obj$mutate_class(x = (a + b) * 2)}
     mutate_class = function(...) {
       dots <- enexprs(...)

       # loops through names and evaluates lazy dot expressions
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
