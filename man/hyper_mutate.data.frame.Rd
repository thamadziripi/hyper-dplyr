% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hyper-mutate.R
\name{hyper_mutate.data.frame}
\alias{hyper_mutate.data.frame}
\title{Hyper Mutate implementation for Data Frames}
\usage{
\method{hyper_mutate}{data.frame}(.data, ...)
}
\arguments{
\item{.data}{A data frame, data frame extension (e.g. a tibble), or a
lazy data frame (e.g. from dbplyr or dtplyr).}

\item{...}{<[`data-masking`][rlang::args_data_masking]> Name-value pairs.
The name gives the name of the column in the output.}
}
\description{
Hyper-mutate relies on a R6 wrapper which imitates the primary behavior of
`dplyr::mutate`. It's very light weight by design and prioritizes
performance by reducing R's copy-on-modify behavior. R6 classes by nature
allow for in-place modifications, reducing the overhead of having to
frequently copy objects to memory.
}
