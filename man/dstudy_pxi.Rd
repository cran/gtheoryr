\name{dstudy_pxi}
\alias{dstudy_pxi}
\title{Design Study for a Crossed Persons-by-Items Design}
\usage{
dstudy_pxi(gstudy, n_items = gstudy$n_items)
}
\arguments{
  \item{gstudy}{A result from \code{gstudy_pxi()}.}
  \item{n_items}{Number of items in the proposed design.}
}
\value{
An object of class \code{"dstudy_gtheoryr"}.
}
\description{
Computes relative error, absolute error, a generalizability coefficient,
and a phi coefficient for a proposed number of items.
}
\examples{
scores <- data.frame(
  person = rep(c("P1", "P2", "P3"), each = 3),
  item = rep(c("I1", "I2", "I3"), times = 3),
  score = c(8, 7, 9, 5, 4, 6, 7, 6, 8)
)

gs <- gstudy_pxi(scores, person = "person", item = "item", score = "score")
dstudy_pxi(gs, n_items = 6)
}
