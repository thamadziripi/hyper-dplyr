# \dontrun{
# # benchmarking example:
# df <- data.frame(a = sample(1:1e7, 1e7), b = sample(1:1e7, 1e7), c = sample(1:1e7, 1e7), d = sample(1:1e7, 1e7))
#
# microbenchmark::microbenchmark(
#   hyperdplyr = hyper_mutate(df, e = a*b, f = c+a, g=exp(f)),
#   dplyr = dplyr::mutate(df, e = a*b, f = c+a, g=exp(f))
#   )
# }

