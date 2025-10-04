box::use(rlang[..., enquos, eval_tidy, sym], glue[...], dplyr[select], htestmod[COR_TEST1, COR_TEST2, COR_TEST3])

# custom_select = function(..., data = NULL) {
#     cols = enquos(...)  
#     select(.data = data, !!!cols)
# }

#' @export
cor = function (..., .data = NULL, alternative = "two.sided", method = "spearman", CI = FALSE, alpha = 0.05, approximate = FALSE) {
    dots = enquos(...) 
    
    # Case 1: No arguments provided
    if (length(dots) == 0) {
        if (is.null(.data)) {
            stop("`.data` must be provided when both `x` and `y` are missing.")
        }
        print(glue('The correlation matrix of the {deparse(substitute(.data))} is: '))
        return(COR_TEST2(as.matrix(.data), alternative = alternative, method = method, CI = CI, alpha = alpha, approximate = approximate))
    }
    
    # Case 2: Vectors provided directly
    if (is.null(.data)) {
        # Evaluate the quoted expressions
        values = lapply(dots, eval_tidy)
        if (all(vapply(values, is.numeric, logical(1)))) {
            return(do.call(COR_TEST3, c(values, list(
                alternative = alternative,
                method = method,
                CI = CI,
                alpha = alpha,
                approximate = approximate
            ))))
        }
    }
    
    # Case 3: Data frame with column names
    if (!is.null(.data)) {
        if (length(dots) == 2) {
            # For two columns, use COR_TEST1
            x = eval_tidy(dots[[1]], data = .data)
            y = eval_tidy(dots[[2]], data = .data)
            return(COR_TEST1(x, y, 
                             alternative = alternative,
                             method = method,
                             CI = CI,
                             alpha = alpha,
                             approximate = approximate
            ))
        } 
        
        # For more than two columns, use custom_select and COR_TEST2
        dat = select(.data, !!!dots)
        return(COR_TEST2(as.matrix(dat),
                         alternative = alternative,
                         method = method,
                         CI = CI,
                         alpha = alpha,
                         approximate = approximate
        ))
    }
}

#' @export
cor_pipe = function (.data = NULL, ..., alternative = "two.sided", method = "spearman", CI = FALSE, alpha = 0.05, approximate = FALSE) {
    # If `.data` is provided
    if (!is.null(.data)) {
        return(COR_TEST2(.data, alternative = alternative, method = method, CI = CI, alpha = alpha, approximate = approximate))
    }
    
    # Capture the column names as expressions
    dots = eval(substitute(alist(...)))
    
    # Convert expressions to character strings
    col_names = sapply(dots, function(x) as.character(x))
    
    # Select the columns and perform correlation
    dat = select(.data, all_of(col_names))
    COR_TEST2(as.matrix(dat), alternative = alternative, method = method, CI = CI, alpha = alpha, approximate = approximate)
}