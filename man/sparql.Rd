% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sparql.R
\name{sparql}
\alias{sparql}
\title{Execute a SPARQL query against a remote endpoint and get the results}
\usage{
sparql(query, endpoint)
}
\arguments{
\item{query}{A single string containing a valid SPARQL query.  Make sure to
include any necessary \code{PREFIX} declarations.}

\item{endpoint}{A string giving the full URL of the SPARQL HTTP endpoint
(e.g. \code{"https://lindas.admin.ch/query"}).}
}
\value{
A tibble holding the results of the query.
}
\description{
Sends a SPARQL \code{SELECT} (or other) query to the specified SPARQL HTTP endpoint,
requests the results in CSV format, and returns them as a tibble in R.
}
\details{
\itemize{
\item The function uses \strong{httr}’s \code{POST(..., encode = "form")} under the hood, which
automatically URL-encodes your \code{query} parameter and sets the
\code{Content-Type: application/x-www-form-urlencoded} header.
\item It also sets \code{Accept: text/csv} so that the endpoint returns CSV.
\item Messages from the underlying CSV parser are suppressed by \code{suppressMessages()}.
}
}
\examples{
query <- '
PREFIX : <https://agriculture.ld.admin.ch/plant-protection/>
PREFIX schema: <http://schema.org/>
SELECT ?name ?wNumber (COUNT(?hCode) AS ?hazards) WHERE {
  ?p a :Product ;
     schema:name ?name ;
     :federalAdmissionNumber ?wNumber ;
     :hasHazardStatement ?hCode .
}
GROUP BY ?name ?wNumber
ORDER BY DESC(?hazards)
'

# run it
data <- sparql(
  query    = query,
  endpoint = "https://lindas.admin.ch/query"
)
print(data)

}
\seealso{
\itemize{
\item \code{\link[httr:POST]{httr::POST()}}, \code{\link[httr:content]{httr::content()}} for low-level HTTP
\item \code{\link[readr:read_delim]{readr::read_csv()}} for parsing CSV into a data.frame/tibble
}
}
