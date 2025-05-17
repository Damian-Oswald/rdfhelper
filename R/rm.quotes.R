rm.quotes <- function(x) {
    x <- base::as.character(x)
    x <- base::sub("\"", "'", x)
    return(x)
}
