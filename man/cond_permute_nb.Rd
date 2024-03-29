% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{cond_permute_nb}
\alias{cond_permute_nb}
\title{Conditional permutation of neighbors}
\usage{
cond_permute_nb(nb, seed = NULL)
}
\arguments{
\item{nb}{a neighbor list.}

\item{seed}{default null. A value to pass to \code{set.seed()} for reproducibility.}
}
\value{
A list of class \code{nb} where each element contains a random sample of neighbors excluding the observed region.
}
\description{
Creates a conditional permutation of neighbors list holding i fixed and shuffling it's neighbors.
}
\examples{
nb <- st_contiguity(guerry)
nb[1:5]
# conditionally permute neighbors
perm_nb <- cond_permute_nb(nb)
perm_nb[1:5]
}
