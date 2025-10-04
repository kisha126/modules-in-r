box::use(
  tibble[as_tibble]
)

#' @export
pi = pi

#' @export
iris = as_tibble(datasets::iris)

#' @export
sales = readRDS(box::file('data/sales.rds'))



