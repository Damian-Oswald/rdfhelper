#' Construct an RDF Triple Statement
#'
#' This helper constructs an RDF triple statement as a formatted string, combining a subject, predicate, and object.
#'
#' @param subject RDF subject (URI or blank node).
#' @param predicate RDF predicate (URI).
#' @param object RDF object (URI, literal, or blank node).
#'
#' @return A formatted RDF triple as a string.
#'
#' @examples
#' triple(uri("https://example.com/1234"), uri("https://example.com/hasName"), literal("Apple"))
#' triple(uri("1234", prefix = "https://example.com/"), uri("rdf:type"), uri("https://example.com/Fruit"))
#'
#' @export
triple <- function(subject, predicate, object) {
    sprintf('\n%s %s %s .', subject, predicate, object)
}
