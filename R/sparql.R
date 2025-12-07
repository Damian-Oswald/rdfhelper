#' Execute a SPARQL query against a remote endpoint and get the results
#'
#' Sends a SPARQL `SELECT` (or other) query to the specified SPARQL HTTP endpoint,
#' requests the results in CSV format, and returns them as a tibble in R.
#'
#' @param query    A single string containing a valid SPARQL query.  Make sure to
#'                 include any necessary `PREFIX` declarations.
#' @param endpoint A string giving the full URL of the SPARQL HTTP endpoint
#'                 (e.g. `"https://lindas.admin.ch/query"`).
#' @param accept   Character string. The MIME type indicating the desired response format 
#'                 from the server. Common values depend on the query type:
#'                 \itemize{
#'                   \item \code{"text/csv"} (Recommended for SELECT): Returns a tabular result that can be parsed into a data frame.
#'                   \item \code{"text/turtle"} (Recommended for CONSTRUCT): Returns an RDF graph in Turtle format.
#'                   \item \code{"application/json"}: Returns the raw JSON response (often used for both SELECT and ASK queries).
#'                   \item \code{"application/rdf+xml"}: An alternative XML format for graph data.
#'                 }
#' 
#' @return A tibble holding the results of the query, or a string containing any non-CSV response.
#'
#' @details
#' - The function uses **httr**’s `POST(..., encode = "form")` under the hood, which
#'   automatically URL-encodes your `query` parameter and sets the
#'   `Content-Type: application/x-www-form-urlencoded` header.
#' - It also sets `Accept: text/csv` so that the endpoint returns CSV.
#' - Messages from the underlying CSV parser are suppressed by `suppressMessages()`.
#'
#' @examples
#' query <- '
#' PREFIX : <https://agriculture.ld.admin.ch/plant-protection/>
#' PREFIX schema: <http://schema.org/>
#' SELECT ?name ?wNumber (COUNT(?hCode) AS ?hazards) WHERE {
#'   ?p a :Product ;
#'      schema:name ?name ;
#'      :federalAdmissionNumber ?wNumber ;
#'      :hasHazardStatement ?hCode .
#' }
#' GROUP BY ?name ?wNumber
#' ORDER BY DESC(?hazards)
#' '
#'
#' # run it
#' data <- sparql(
#'   query    = query,
#'   endpoint = "https://lindas.admin.ch/query"
#' )
#' print(data)
#' 
#' # run a CONSTRUCT query
#' sparql('
#'   CONSTRUCT { ?s ?p ?o . }
#'   WHERE { ?s ?p ?o . }
#'   LIMIT 10',
#'   endpoint = "https://lindas.admin.ch/query",
#'   accept = "text/turtle"
#' )
#'
#' @seealso
#' - [httr::POST()], [httr::content()] for low-level HTTP
#' - [readr::read_csv()] for parsing CSV into a data.frame/tibble
#'
#' @importFrom httr POST add_headers content
#' @export
sparql <- function(query, endpoint, accept = "text/csv")
{
    httr::POST(
        url = endpoint,
        httr::add_headers(Accept = accept),
        body = list(query = query),
        encode = "form"
    ) |>
        httr::content(encoding = "UTF-8") |>
        base::suppressMessages()
}
