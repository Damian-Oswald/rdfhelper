% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/literal.R
\name{literal}
\alias{literal}
\title{Construct an RDF Literal}
\usage{
literal(x)
}
\arguments{
\item{x}{Character string. The textual content of the literal.}
}
\value{
A formatted RDF plain literal.
}
\description{
This helper constructs a plain RDF literal (simple string literal without language tag or datatype).
}
\examples{
literal("Hello, World!")

}
