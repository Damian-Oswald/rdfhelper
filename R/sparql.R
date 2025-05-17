#' Execute a SPARQL query against a remote endpoint and get the results
#'
#' Sends a SPARQL `SELECT` (or other) query to the specified SPARQL HTTP endpoint,
#' requests the results in CSV format, and returns them as a tibble in R.
#'
#' @param query    A single string containing a valid SPARQL query.  Make sure to
#'                 include any necessary `PREFIX` declarations.
#' @param endpoint A string giving the full URL of the SPARQL HTTP endpoint
#'                 (e.g. `"https://lindas.admin.ch/query"`).
#'
#' @return A tibble holding the results of the query.
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
#' @seealso
#' - [httr::POST()], [httr::content()] for low-level HTTP
#' - [readr::read_csv()] for parsing CSV into a data.frame/tibble
#'
#' @importFrom httr POST add_headers content
#' @export
sparql <- function(query, endpoint)
{
    httr::POST(
        url = endpoint,
        httr::add_headers(Accept = "text/csv"),
        body = list(query = query),
        encode = "form"
    ) |>
        httr::content(encoding = "UTF-8") |>
        base::suppressMessages()
}
