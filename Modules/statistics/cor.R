#' @export
custom_cor <- function(x, y) {
    if(length(x) != length(y)) {
        stop("x and y must have the same length.")
    }
    
    x_centered <- x - mean(x)
    y_centered <- y - mean(y)
    
    dot_product <- sum(x_centered * y_centered)
    
    norm_x <- sqrt(sum(x_centered^2))
    norm_y <- sqrt(sum(y_centered^2))
    
    r <- dot_product / (norm_x * norm_y)
    
    return(r)
}