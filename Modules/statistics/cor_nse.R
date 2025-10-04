box::use(rlang[..., enquos, eval_tidy, sym], glue[...], dplyr[select], stats[cor])

#' @export
corr = function (..., data = NULL, method = c("pearson", "kendall", "spearman"), use = "everything") {
  dots = enquos(...) 
  method = match.arg(method)
  
  if (length(dots) == 0) {
    if (is.null(data)) {
      stop("`.data` must be provided when both `x` and `y` are missing.")
    }
    print(glue('The correlation matrix of the {deparse(substitute(data))} is: '))
    return(cor(as.matrix(data), method = method, use = use))
  }
  
  if (is.null(data)) {
    values = lapply(dots, eval_tidy)
    
    if (all(vapply(values, is.numeric, logical(1)))) {
      if (length(values) == 2) {
        x = values[[1]]
        y = values[[2]]
        return(cor(x, y, method = method, use = use))
      } else {
        return(cor(do.call(cbind, values), method = method, use = use))
      }
    } else {
      stop("All arguments must be numeric.")
    }
  }
  
  if (!is.null(data)) {
    if (length(dots) == 2) {
      x = eval_tidy(dots[[1]], data = data)
      y = eval_tidy(dots[[2]], data = data)
      return(cor(x, y, method = method, use = use))
    } 
    
    # Check if data is NULL before using select
    if (is.null(data)) {
      stop("Data must be provided when selecting multiple columns.")
    }
    dat = select(data, !!!dots)
    return(cor(as.matrix(dat), method = method, use = use))
  }
}

#' @export
cor_pipe = function (.data, ..., alternative = "two.sided", method = c("pearson", "kendall", "spearman"), CI = FALSE, alpha = 0.05, approximate = FALSE) {
  method = match.arg(method)
  
  # Ensure .data is provided
  if (is.null(.data)) {
    stop("`.data` must be provided, it cannot be NULL in this function.")
  }
  
  dots = eval(substitute(alist(...)))
  
  if (length(dots) == 0) {
    # If no specific columns are provided, compute correlation for all columns in .data
    return(cor(as.matrix(.data), method = method))
  }
  
  # Convert expressions (column names) to character strings
  col_names = sapply(dots, as.character)
  
  # Select specified columns from .data
  dat = select(.data, all_of(col_names))
  
  # Compute the correlation matrix
  return(cor(as.matrix(dat), method = method))
}

