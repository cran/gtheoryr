\name{gstudy_pxif}
\alias{gstudy_pxif}
\title{Estimate Variance Components for a Crossed Persons-by-Items-by-Facet Design}
\usage{
gstudy_pxif(data, person, item, facet, score, facet_name = facet)
}
\arguments{
  \item{data}{A data frame containing one row per person-item-facet observation.}
  \item{person}{Name of the person column.}
  \item{item}{Name of the item column.}
  \item{facet}{Name of the third facet column, such as a rater, occasion, or form.}
  \item{score}{Name of the numeric score column.}
  \item{facet_name}{A user-facing label for the third facet. Defaults to the value supplied to \code{facet}.}
}
\value{
An object of class \code{"gstudy_gtheoryr"}.
}
\description{
Estimates ANOVA mean squares and variance components for a fully crossed
random-effects design with persons crossed with items and one additional
facet such as raters, occasions, or forms.
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
gs
}
