% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spacetime-nb-wt.R
\name{spt_wt}
\alias{spt_wt}
\title{Create time lagged spatial weights}
\usage{
spt_wt(wt, nbt, n_times, n_locs, k)
}
\arguments{
\item{wt}{a weights lists created from a spacetime object's geometry context.}

\item{nbt}{a time-lagged spatial neighbors list created by \code{\link[=spt_nb]{spt_nb()}}}

\item{n_times}{the number of time slices}

\item{n_locs}{the number of locations}
}
\value{
a weight list with same cardinality as \code{nbt}
}
\description{
Given a space-time-lagged neighbor list and a weights lists, e.g. made by
\code{\link[=st_dist_band]{st_dist_band()}} or \code{\link[=st_contiguity]{st_contiguity()}}, create a corresponding weights list.
}
\details{
It is intended that \code{\link[=spt_wt]{spt_wt()}} is used following the use of \code{\link[=spt_nb]{spt_nb()}}
using the same input arguments. This ensures that the time-lagged spatial
weights are correctly associated with the time-lagged spatial neighbors.
}
\seealso{
\code{\link[=spt_nb]{spt_nb()}} for creating spacetime neighbors which are inputs into spacetime weights
}
\keyword{internal}
