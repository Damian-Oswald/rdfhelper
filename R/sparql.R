#' Execute a SPARQL query against a remote endpoint and get the results
#'
#' Sends a SPARQL `SELECT` (or other) query to the specified SPARQL HTTP endpoint,
#' requests the results in CSV format, and returns them as a character string.
#' You can then parse the CSV (e.g. with **readr**’s `read_csv()`) into a data frame
#' for further processing.
#'
#' @param query   A single string containing a valid SPARQL query.  Make sure to
#'                include any necessary PREFIX declarations.
#' @param endpoint A string giving the full URL of the SPARQL HTTP endpoint
#'                 (e.g. `"https://lindas.admin.ch/query"`).
#'
#' @return A single-length character vector holding the CSV-formatted results.
#'         To convert to a data frame, you can do:
#'         ```r
#'         raw_csv <- sparql(q, endpoint)
#'         df <- readr::read_csv(raw_csv, show_col_types = FALSE)
#'         ```
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
#' sparql(query, "https://lindas.admin.ch/query")
#'
#' @seealso
#' - [httr::POST()], [httr::content()] for low-level HTTP  
#' - [readr::read_csv()] for parsing CSV into a data.frame/tibble
#'
#' @importFrom httr POST add_headers content_type content
#' @export
sparql <- function(query, endpoint) {
    query <- POST(endpoint,
                  add_headers("Accept" = "text/csv"),
                  content_type("application/x-www-form-urlencoded; charset=UTF-8"),
                  body = paste("query=", query, sep = ""))
    suppressMessages({
        data <- content(query, encoding = "UTF-8")
    })
    return(data)
}
