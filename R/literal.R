#' Construct an RDF Literal
#'
#' This helper constructs a plain RDF literal (simple string literal without language tag or datatype).
#'
#' @param x Character string. The textual content of the literal.
#'
#' @return A formatted RDF plain literal.
#'
#' @examples
#' literal("Hello, World!")
#'
#' @export
literal <- function(x) {
    if (rdfhelper:::is.missing(x))
    {
        return(NULL)
    }
    x |> 
        rdfhelper:::rm.quotes() |>
        base::sprintf('"%s"', ... = _)
}
