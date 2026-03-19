\name{gstudy_pxi}
\alias{gstudy_pxi}
\title{Estimate Variance Components for a Crossed Persons-by-Items Design}
\usage{
gstudy_pxi(data, person, item, score)
}
\arguments{
  \item{data}{A data frame containing one row per person-item observation.}
  \item{person}{Name of the person column.}
  \item{item}{Name of the item column.}
  \item{score}{Name of the numeric score column.}
}
\value{
An object of class \code{"gstudy_gtheoryr"}.
}
\description{
Estimates ANOVA mean squares and variance components for a fully crossed
random-effects persons-by-items design.
}
\examples{
scores <- data.frame(
  person = rep(c("P1", "P2", "P3"), each = 3),
  item = rep(c("I1", "I2", "I3"), times = 3),
  score = c(8, 7, 9, 5, 4, 6, 7, 6, 8)
)

gs <- gstudy_pxi(scores, person = "person", item = "item", score = "score")
gs
}
