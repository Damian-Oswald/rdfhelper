#' Execute a SPARQL query against a remote endpoint and get the results
#'
#' Sends a SPARQL `SELECT` (or other) query to the specified SPARQL HTTP endpoint,
#' requests the results in CSV format, and returns them as a tibble in R.
#'
#' @param query   A single string containing a valid SPARQL query.  Make sure to
#'                include any necessary PREFIX declarations.
#' @param endpoint A string giving the full URL of the SPARQL HTTP endpoint
#'                 (e.g. `"https://lindas.admin.ch/query"`).
#'
#' @return A tibble holding the results of the query.
#'
#' @details
#' - The function uses **httr**’s `POST()` under the hood, setting `Accept: text/csv`
#'   so that the endpoint returns CSV.  
#' - Messages from the underlying CSV parser (if you choose to pipe into `read_csv()`)
#'   are suppressed by `suppressMessages()`.  
#' - If you’d rather get a parsed data frame directly, wrap the call in
#'   `readr::read_csv(..., show_col_types = FALSE)`.
#'
#' @examples
#' # build your query string, e.g.:
#' query <- '
#' PREFIX : <https://agriculture.ld.admin.ch/plant-protection/>
#' PREFIX schema: <http://schema.org/>
#' SELECT ?name ?wNumber (COUNT(?hCode) AS ?hazards) WHERE {
#'   ?p a :Product ;
#'     schema:name ?name ;
#'     :federalAdmissionNumber ?wNumber ;
#'     :hasHazardStatement ?hCode .
#' }
#' GROUP BY ?name ?wNumber
#' ORDER BY DESC(?hazards)
#' '
#'
#' # run it
#' data <- sparql(
#'     query = query,
#'     endpoint = , "https://lindas.admin.ch/query"
#' )
#' print(data)
#' @seealso
#' - [httr::POST()], [httr::content()] for low-level HTTP  
#' - [readr::read_csv()] for parsing CSV into a data.frame/tibble
#'
#' @importFrom httr POST add_headers content_type content
#' @export
sparql <- function(query, endpoint) {
    query <- httr::POST(
        url = endpoint,
        httr::add_headers("Accept" = "text/csv"),
        httr::content_type("application/x-www-form-urlencoded; charset=UTF-8"),
        body = paste("query=", query, sep = "")
        )
    base::suppressMessages({
        data <- httr::content(query, encoding = "UTF-8")
    })
    return(data)
}
