#' @export
moving_average_r = function(x, win = 3) {
    nn = length(x)
    ll = nn - win + 1
    res = numeric(ll)
    
    for (i in 1:ll) {
        res[i] = mean(x[i:(i + win - 1)])
    }
    res
}
