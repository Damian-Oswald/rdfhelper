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
    
    allowed <- c("string",
                 "integer",
                 "decimal",
                 "boolean",
                 "date",
                 "dateTime")
    xsd <- "http://www.w3.org/2001/XMLSchema"
    
    if (rdfhelper:::is.missing(x))
    {
        return(NULL)
    }
    else if (base::is.null(datatype))
    {
        stop("A datatype must be provided.")
    }
    else if (!datatype %in% allowed)
    {
        stop(
            allowed_types |>
                base::paste(collapse = ", ") |>
                base::sprintf("`datatype` must be one of: %s", ... = _)
        )
    }
    x |>
        rdfhelper::literal() |>
        base::sprintf('%s^^<%s#%s>', ... = _, xsd, datatype)
}
