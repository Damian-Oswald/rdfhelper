#' Construct a Typed RDF Literal
#'
#' This helper constructs a typed RDF literal, associating a value with any specified
#' XML Schema datatype (`xsd`) or GeoSPARQL datatype.
#'
#' @details
#' Common datatypes include, but are not limited to:
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
#' **Spatial Types:**
#' `wktLiteral`
#'
#' **Other Types:**
#' `boolean`, `base64Binary`, `hexBinary`, `anyURI`
#'
#' @param x The value to be typed. It will be coerced to a character string.
#' @param datatype Character string. A valid XML Schema or GeoSPARQL datatype name (e.g., `"string"`, `"integer"`, `"wktLiteral"`).
#'   This parameter must be provided.
#'
#' @return A formatted RDF typed literal string (e.g., `"42"^^<...#integer>`).
#'   If `x` is `NA`, `NULL`, or `""`, the function returns `NULL` to avoid creating
#'   invalid literals.
#'
#' @examples
#' typed("42", "integer")
#' typed("true", "boolean")
#' typed("POINT(30 10)", "wktLiteral")
#' typed("", "integer") # Returns NULL
#'
#' @export
typed <- function(x, datatype = NULL) {
    
    # Define XML Schema Types
    xsd_types <- c(
        "string", "normalizedString", "token", "language", "Name", "NCName", 
        "ID", "IDREF", "decimal", "integer", "nonPositiveInteger", 
        "negativeInteger", "long", "int", "short", "byte", "nonNegativeInteger", 
        "positiveInteger", "unsignedLong", "unsignedInt", "unsignedShort", 
        "unsignedByte", "double", "float", "dateTime", "dateTimeStamp", 
        "date", "time", "duration", "gYearMonth", "gYear", "gMonthDay", 
        "gDay", "gMonth", "boolean", "base64Binary", "hexBinary", "anyURI"
    )
    
    # Define GeoSPARQL Types
    geo_types <- c("wktLiteral")
    
    # Combine all allowed types
    allowed <- c(xsd_types, geo_types)
    
    # Define Namespaces
    ns_xsd <- "http://www.w3.org/2001/XMLSchema"
    ns_geo <- "http://www.opengis.net/ont/geosparql"
    
    if (rdfhelper:::is.missing(x)) {
        return(NULL)
    } else if (base::is.null(datatype)) {
        stop("A datatype must be provided.")
    } else if (!datatype %in% allowed) {
        stop(
            allowed |>
                base::paste(collapse = ", ") |>
                base::sprintf("`datatype` must be one of: %s", ... = _)
        )
    }
    
    # Determine which namespace to use based on the datatype
    if (datatype %in% geo_types) {
        selected_ns <- ns_geo
    } else {
        selected_ns <- ns_xsd
    }
    
    x |>
        rdfhelper::literal() |>
        base::sprintf('%s^^<%s#%s>', ... = _, selected_ns, datatype)
}
