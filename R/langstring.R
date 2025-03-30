#' Construct a Language-tagged RDF literal
#'
#' This helper constructs an RDF language-tagged literal (`rdf:langString`), which is a string explicitly associated with a language tag following BCP47 conventions (e.g., `"Apple"@en`, `"Apfel"@de-CH`).
#'
#' @param x Character string. The textual content of the literal.
#' @param lang Character string. The language tag conforming to [BCP47](https://tools.ietf.org/html/bcp47). Must be provided.
#'
#' @return A formatted RDF language-tagged literal (`rdf:langString`).
#'
#' @examples
#' langstring("Apple", "en")
#' langstring("Grüezi", "de-CH")
#'
#' @export
langstring <- function(x, lang = NULL) {
    if (is.null(lang)) stop("A language tag ('lang') must be provided.")
    sprintf('"%s"@%s', x, lang)
}
