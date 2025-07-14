#' Construct a Language-tagged RDF literal
#'
#' This helper constructs an RDF language-tagged literal (`rdf:langString`), which is a string explicitly associated with a language tag following BCP47 conventions (e.g., `"Apple"@en`, `"Apfel"@de-CH`).
#'
#' @param x Character string. The textual content of the literal.
#' @param lang Character string. The language tag conforming to [BCP47](https://tools.ietf.org/html/bcp47). Must be provided.
#' @param multiline Logical. Should the language-tagged string be enclosed with triple-double-qoutes (e.g. `"""Hello, world"""@en`)? `TRUE` allows line breaks within `x`.
#'
#' @return A formatted RDF language-tagged literal (`rdf:langString`).
#'
#' @examples
#' langstring("Apple", "en")
#' langstring("Grüezi", "de-CH")
#'
#' @export
langstring <- function(x, lang = NULL, multiline = FALSE) {
  if (base::is.null(lang))
  {
    stop("A language tag ('lang') must be provided.")
  }
  if (base::is.null(x) || base::is.na(x) || x=="")
  {
    return(NULL)
  }
  s <- x |>
    base::as.character() |>
    base::gsub("\"", "'", x = _)
  if (multiline)
  {
    base::sprintf('"""\n%s\n"""@%s', s, lang)
  }
  else
  {
    base::sprintf('"%s"@%s', s, lang)
  }
}
