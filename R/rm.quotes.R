rm.quotes <- function(x) {
    x |>
        base::as.character() |>
        base::sub("\"", "'", x = _)
}
