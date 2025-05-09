% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/uri.R
\name{uri}
\alias{uri}
\title{URI constructor}
\usage{
uri(x, prefix = NULL)
}
\arguments{
\item{x}{String. The URL/URI itself, or a part of it, if a prefix is defined.}

\item{prefix}{String. A prefix to a URL, e.g. \verb{https://example.com/}.}
}
\description{
Construct a URI as one string.
}
\examples{
uri("https://example.com/1234")
uri("1234", prefix = "https://example.com/")
}
