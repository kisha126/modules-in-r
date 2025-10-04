box::use(
  tibble[as_tibble]
)

#' @export
pi = pi

#' @export
iris = as_tibble(datasets::iris)

#' @export
sales = readRDS(box::file('data/sales.rds'))

#' @export
name_md = paste0("Name of the module: ", box::name())

