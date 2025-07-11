#' Construct an RDF Triple Statement
#'
#' This helper constructs an RDF triple statement as a formatted string, combining a subject, predicate, and object.
#'
#' @param subject RDF subject (URI or blank node).
#' @param predicate RDF predicate (URI).
#' @param object RDF object (URI, literal, or blank node).
#' @param return Should the triple be returned (or printed)?
#'
#' @return A formatted RDF triple as a string.
#'
#' @examples
#' triple(uri("https://example.com/1234"), uri("https://example.com/hasName"), literal("Apple"))
#' triple(uri("1234", prefix = "https://example.com/"), uri("rdf:type"), uri("https://example.com/Fruit"))
#' triple("https://example.com/1", "https://example.com/2", NULL)
#' triple("https://example.com/1", "https://example.com/2", "")
#'
#' @export
triple <- function(
        subject,
        predicate,
        object,
        return = FALSE
)
{
    for (i in list(subject, predicate, object))
    {
        if(rdfhelper:::is.missing(head(i, 1)))
        {
            return(NULL)
        }
    }
    x <- base::sprintf("%s %s %s .\n",
                       subject,
                       predicate,
                       object)
    if(return)
    {
        return(x)
    }
    else
    {
        base::cat(x)
    }
}
