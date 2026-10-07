\name{gstudy_pxiro}
\alias{gstudy_pxiro}
\alias{dstudy_pxiro}
\title{Helpers for a Crossed Persons-by-Items-by-Raters-by-Occasions Design}
\usage{
gstudy_pxiro(data, person, item, rater, occasion, score)

dstudy_pxiro(gstudy, n_items = gstudy$design_levels[["item"]],
  n_raters = gstudy$design_levels[["rater"]],
  n_occasions = gstudy$design_levels[["occasion"]])
}
\arguments{
  \item{data}{A data frame containing one row per person-item-rater-occasion observation.}
  \item{person}{Name of the person column.}
  \item{item}{Name of the item column.}
  \item{rater}{Name of the rater column.}
  \item{occasion}{Name of the occasion column.}
  \item{score}{Name of the numeric score column.}
  \item{gstudy}{A result from \code{gstudy_pxiro()}.}
  \item{n_items}{Number of items in the design. Defaults to the current number.}
  \item{n_raters}{Number of raters in the design. Defaults to the current number.}
  \item{n_occasions}{Number of occasions in the design. Defaults to the current number.}
}
\value{
For \code{gstudy_pxiro()}, an object of class \code{"gstudy_gtheoryr"}.

For \code{dstudy_pxiro()}, an object of class \code{"dstudy_gtheoryr"}.
}
\description{
Convenience wrappers around \code{gstudy_crossed()} and \code{dstudy_crossed()}
for a design with persons, items, raters, and occasions.
}
\examples{
scores <- read.csv(
  system.file("extdata", "crossed_scores_rater_occasion.csv", package = "gtheoryr"),
  stringsAsFactors = FALSE
)

gs <- gstudy_pxiro(
  scores,
  person = "person",
  item = "item",
  rater = "rater",
  occasion = "occasion",
  score = "score"
)
gs
dstudy_pxiro(gs)
}
