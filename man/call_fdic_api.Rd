% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fdic_api_req.R
\name{call_fdic_api}
\alias{call_fdic_api}
\title{Call FDIC API}
\usage{
call_fdic_api(
  api = NULL,
  filters = NULL,
  search = NULL,
  fields = NULL,
  sort_by = NULL,
  sort_order = NULL,
  limit = 0,
  offset = NULL,
  agg_by = NULL,
  agg_term_fields = NULL,
  format = NULL,
  download = NULL,
  filename = NULL
)
}
\arguments{
\item{api}{The API to call}

\item{filters}{A list of filters to apply to the query}

\item{search}{A list of search terms to apply to the query}

\item{fields}{A list of fields to return}

\item{sort_by}{A list of fields to sort by}

\item{sort_order}{A list of sort orders}

\item{limit}{The number of records to return}

\item{offset}{The number of records to skip}

\item{agg_by}{A list of fields to aggregate by}

\item{agg_term_fields}{A list of fields to aggregate terms by}

\item{format}{The format to return the data in}

\item{download}{Whether to download the data}

\item{filename}{The filename to download the data to}
}
\value{
A list of query responses from HTTR2
}
\description{
Call the FDIC API
}
\examples{
call_fdic_api(api = "financials", filters = "RSSDID: 37", fields = "RSSDID,REPDTE,ASSET,DEP", limit = 10)
}
