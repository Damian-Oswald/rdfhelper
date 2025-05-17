#' Generate Nano IDs
#'
#' Quickly create random, URL-safe identifier strings ("nano IDs").
#' The generator is **not** cryptographically secure, but it is fast and
#' compact—ideal for temporary keys, file names, etc.
#'
#' @param n Integer scalar. How many IDs to generate.  
#'   Must be a positive whole number. Default is `1`.
#' @param length Integer scalar. Total length **including** any `prefix`.  
#'   Default is `16`.
#' @param prefix Single string (possibly empty) to prepend to every ID.  
#'   Must be shorter than `length`.
#' @param characters Character vector from which the random part is drawn.  
#'   Defaults to `c(LETTERS, letters, 0:9)`.
#'
#' @return A character vector of length `n` containing the generated IDs.
#'
#' @examples
#' nano()
#' nano(24)
#' nano(prefix = "Q")
#' nano(5, prefix = "Q", characters = 0:9)
#' nano(n = 10)
#'
#' @export
nano <- function(
        n = 1,
        length = 16,
        prefix = "",
        characters = c(base::LETTERS, base::letters, 0:9)
) {
    if (!base::is.character(prefix) || base::length(prefix) != 1L) {
        stop("`prefix` must be a single character string.")
    }
    p <- base::nchar(prefix, type = "chars")
    if (p >= length) {
        stop("`prefix` must be shorter than `length`.")
    }
    if (!base::is.numeric(n) || base::length(n) != 1L ||
        n < 1 || n != base::as.integer(n)) {
        stop("`n` must be a positive integer scalar.")
    }
    make_one <- function() {
        random <- base::sample(characters, size = length - p, replace = TRUE)
        base::paste(base::c(prefix, random), collapse = "")
    }
    base::vapply(seq_len(n), function(i) make_one(), base::character(1))
}
