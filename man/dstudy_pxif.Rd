\name{dstudy_pxif}
\alias{dstudy_pxif}
\title{Design Study for a Crossed Persons-by-Items-by-Facet Design}
\usage{
dstudy_pxif(gstudy, n_items = gstudy$n_items, n_facets = gstudy$n_facets)
}
\arguments{
  \item{gstudy}{A result from \code{gstudy_pxif()}.}
  \item{n_items}{Number of items in the design. Defaults to the current number of items in the supplied \code{gstudy} object.}
  \item{n_facets}{Number of levels of the third facet. Defaults to the current number in the supplied \code{gstudy} object.}
}
\value{
An object of class \code{"dstudy_gtheoryr"}.
}
\description{
Computes relative and absolute error variances, the generalizability
coefficient, and the phi coefficient for the current or proposed number of
items and levels of a third crossed facet such as raters, occasions, or forms.
}
\examples{
scores <- expand.grid(
  person = c("P1", "P2", "P3"),
  item = c("I1", "I2"),
  rater = c("R1", "R2"),
  stringsAsFactors = FALSE
)
scores$score <- c(8, 7, 7, 6, 5, 4, 6, 5, 7, 6, 8, 7)

gs <- gstudy_pxif(
  scores,
  person = "person",
  item = "item",
  facet = "rater",
  score = "score",
  facet_name = "rater"
)

dstudy_pxif(gs)
}
