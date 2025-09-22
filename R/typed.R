#' Construct a Typed RDF Literal
#'
#' This helper constructs a typed RDF literal, associating a value with any specified
#' XML Schema datatype (`xsd`). It is generalized to accept any valid XSD datatype string.
#'
#' @details
#' Common XSD datatypes include, but are not limited to:
#'
#' **String Types:**
#' `string`, `normalizedString`, `token`, `language`, `Name`, `NCName`, `ID`, `IDREF`
#'
#' **Numeric Types:**
#' `decimal`, `integer`, `nonPositiveInteger`, `negativeInteger`, `long`, `int`, `short`, `byte`,
#' `nonNegativeInteger`, `positiveInteger`, `unsignedLong`, `unsignedInt`, `unsignedShort`,
#' `unsignedByte`, `double`, `float`
#'
#' **Date and Time Types:**
#' `dateTime`, `dateTimeStamp`, `date`, `time`, `duration`, `gYearMonth`, `gYear`, `gMonthDay`,
#' `gDay`, `gMonth`
#'
#' **Other Types:**
#' `boolean`, `base64Binary`, `hexBinary`, `anyURI`
#'
#' @param x The value to be typed. It will be coerced to a character string.
#' @param datatype Character string. A valid XML Schema datatype name (e.g., `"string"`, `"integer"`).
#'   This parameter must be provided.
#'
#' @return A formatted RDF typed literal string (e.g., `"42"^^<...#integer>`).
#'   If `x` is `NA`, `NULL`, or `""`, the function returns `NULL` to avoid creating
#'   invalid literals.
#'
#' @examples
#' typed("42", "integer")
#' typed("true", "boolean")
#' typed("2025-09-22", "date")
#' typed(5.42, "double")
#' typed("2025-09", "gYearMonth")
#' typed("", "integer") # Returns NULL
#'
#' @export
typed <- function(x, datatype = NULL) {
    
    allowed <- c(
        "string",
        "normalizedString",
        "token",
        "language",
        "Name",
        "NCName",
        "ID",
        "IDREF",
        "decimal",
        "integer",
        "nonPositiveInteger",
        "negativeInteger",
        "long",
        "int",
        "short",
        "byte",
        "nonNegativeInteger",
        "positiveInteger",
        "unsignedLong",
        "unsignedInt",
        "unsignedShort",
        "unsignedByte",
        "double",
        "float",
        "dateTime",
        "dateTimeStamp",
        "date",
        "time",
        "duration",
        "gYearMonth",
        "gYear",
        "gMonthDay",
        "gDay",
        "gMonth",
        "boolean",
        "base64Binary",
        "hexBinary",
        "anyURI"
    )
    
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
