#' URI constructor
#'
#' Construct a URI as one string.
#'
#' @param x String. The URL/URI itself, or a part of it, if a prefix is defined.
#' @param prefix String. A prefix to a URL, e.g. `https://example.com/`.
#' @examples
#' uri("https://example.com/1234")
#' uri("1234", prefix = "https://example.com/")
#' @export
uri <- function(x, prefix = NULL)
{
    if(is.null(prefix))
    {
        prefix <- ""
    }
    base::sprintf("<%s%s>", prefix, x)
}
