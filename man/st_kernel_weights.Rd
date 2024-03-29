% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/weights-kernel.R
\name{st_kernel_weights}
\alias{st_kernel_weights}
\title{Calculate Kernel Weights}
\usage{
st_kernel_weights(
  nb,
  geometry,
  kernel = "uniform",
  threshold = critical_threshold(geometry),
  adaptive = FALSE,
  self_kernel = FALSE
)
}
\arguments{
\item{nb}{an object of class \code{nb} e.g. created by \code{\link[=st_contiguity]{st_contiguity()}} or \code{\link[=st_knn]{st_knn()}}.}

\item{geometry}{the geometry an sf object.}

\item{kernel}{One of "uniform", "gaussian",  "triangular", "epanechnikov", or "quartic". See \link{kernels} for more.}

\item{threshold}{a scaling threshold to be used in calculating}

\item{adaptive}{default \code{FALSE}. If \code{TRUE} uses the maximum neighbor distance for each region as the threshold. Suppresses the \code{threshold} argument.}

\item{self_kernel}{default \code{FALSE}. If \code{TRUE} applies the kernel function to the observed region.}
}
\value{
a list where each element is a numeric vector.
}
\description{
Create a weights list using a kernel function.
}
\details{
By default \code{st_kernel_weight()} utilizes a critical threshold of the maximum neighbor distance using \code{critical_threshold()}. If desired, the critical threshold can be specified manually. The \code{threshold} will be passed to the underlying kernel.
}
\examples{
geometry <- sf::st_geometry(guerry)
nb <- st_contiguity(geometry)
nb <- include_self(nb)
res <- st_kernel_weights(nb, geometry)
head(res, 3)
}
\seealso{
Other weights: 
\code{\link{st_inverse_distance}()},
\code{\link{st_nb_dists}()},
\code{\link{st_weights}()}
}
\concept{weights}
