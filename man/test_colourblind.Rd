% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/test_colourblind.R
\name{test_colourblind}
\alias{test_colourblind}
\title{Test colours for colour blindness}
\usage{
test_colourblind(
  colours,
  mode = c("deutan", "protan", "tritan"),
  smallestPercievableDistance = 9.2,
  ratioThres = 5
)
}
\arguments{
\item{colours}{The colours you want to check}

\item{mode}{The tests you want to perform #'}

\item{smallestPercievableDistance}{Smallest perceivable distance threshold, Default: 9.2}

\item{ratioThres}{Ratio threshold, Default: 5}
}
\value{
Returns a message to the console if any colours used in a plot are potentially not colour-bind safe.
}
\description{
Pass a vector of colours and find out if they are colour blind safe
}
\details{
Test colours for colour blindness

Pass a vector of colours and find out if they are colour blind safe

Pass a vector of colours and find out if they are colour blind safe
}
\examples{
test_colourblind(c("red", "green", "darkgreen"), mode = "protan")

}
\seealso{
\code{\link[utils]{combn}}
 \code{\link[khroma]{compare}}
 \code{\link[colorspace]{simulate_cvd}}
}
