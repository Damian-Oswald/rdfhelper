#' Construct a Typed RDF Literal
#'
#' This helper constructs a typed RDF literal, associating a string value with a specific XML Schema datatype (`xsd`).
#'
#' Supported datatypes are: `"string"`, `"integer"`, `"decimal"`, `"boolean"`, `"date"`, `"dateTime"`.
#'
#' @param x Character or numeric value. The content to be typed.
#' @param datatype Character string. The XML Schema datatype (e.g., `"string"`, `"integer"`). Must be provided.
#'
#' @return A formatted RDF typed literal. If `x` is any of `NA`, `NULL`, or `""`, the return value will be `NULL`.
#' (To avoid things like `"NA"^^<http://www.w3.org/2001/XMLSchema#integer>` as a result of `typed(NA, "integer")`)
#'
#' @examples
#' typed("42", "integer")
#' typed("true", "boolean")
#' typed("2025-03-30", "date")
#' typed("", "integer") # this will return `NULL`
#'
#' @export
typed <- function(x, datatype = NULL) {
    allowed_types <- c("string", "integer", "decimal", "boolean", "date", "dateTime")
    if (is.null(x) || is.na(x) || x=="") return(NULL)
    if (is.null(datatype)) stop("A datatype must be provided.")
    if (!datatype %in% allowed_types) {
        stop(sprintf("Datatype must be one of: %s", paste(allowed_types, collapse = ", ")))
    }
    return(sprintf('%s^^<http://www.w3.org/2001/XMLSchema#%s>', literal(x), datatype))
}
