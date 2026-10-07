\name{gstudy_pxir}
\alias{gstudy_pxir}
\alias{gstudy_pxio}
\title{Convenience Wrappers for Common Three-Facet Designs}
\usage{
gstudy_pxir(data, person, item, rater, score)

gstudy_pxio(data, person, item, occasion, score)
}
\arguments{
  \item{data}{A data frame containing one row per observation.}
  \item{person}{Name of the person column.}
  \item{item}{Name of the item column.}
  \item{rater}{Name of the rater column.}
  \item{occasion}{Name of the occasion column.}
  \item{score}{Name of the numeric score column.}
}
\value{
An object of class \code{"gstudy_gtheoryr"}.
}
\description{
Convenience wrappers around \code{gstudy_pxif()} for crossed persons-by-items-by-raters
and crossed persons-by-items-by-occasions designs.
}
\examples{
scores_rater <- expand.grid(
  person = c("P1", "P2", "P3"),
  item = c("I1", "I2"),
  rater = c("R1", "R2"),
  stringsAsFactors = FALSE
)
scores_rater$score <- c(8, 7, 7, 6, 5, 4, 6, 5, 7, 6, 8, 7)

gstudy_pxir(
  scores_rater,
  person = "person",
  item = "item",
  rater = "rater",
  score = "score"
)

scores_occasion <- expand.grid(
  person = c("P1", "P2", "P3"),
  item = c("I1", "I2"),
  occasion = c("T1", "T2"),
  stringsAsFactors = FALSE
)
scores_occasion$score <- c(8, 8, 7, 7, 5, 5, 6, 6, 7, 7, 8, 8)

gstudy_pxio(
  scores_occasion,
  person = "person",
  item = "item",
  occasion = "occasion",
  score = "score"
)
}
