% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_query-internal.R
\name{get_query}
\alias{get_query}
\title{get_query}
\usage{
get_query(query, type = "apa", get_address = F, get_xy = F)
}
\arguments{
\item{query}{The URL specifying the query}

\item{type}{What type of thing you want to look up on craiglist.  Currently
only apartment searches are available.  Default is \code{apa} for "apartment".}

\item{get_address}{Logical specifying whether to extract address from posting.
Requires reading html of post URL. Default is \code{FALSE}}

\item{get_xy}{Logical specifying whether to extract latitude and longitude from posting.
Requires reading html of post URL. Default is \code{FALSE}. Includes field on lat/lon accuracy}
}
\value{

}
\description{
Query Craigslist and clean the scrape data
}
\keyword{internal}
