\name{variance_proportions_table}
\alias{variance_proportions_table}
\alias{variance_proportions_table.gstudy_gtheoryr}
\title{Variance Component Proportions from a G-study}
\usage{
variance_proportions_table(x, ...)
}
\arguments{
  \item{x}{A \code{"gstudy_gtheoryr"} object.}
  \item{...}{Unused.}
}
\value{
A data frame.
}
\description{
Returns the estimated variance components together with their proportion of the
sum of estimated variance components.
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

variance_proportions_table(gs)
}
