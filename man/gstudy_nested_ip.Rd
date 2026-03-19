\name{gstudy_nested_ip}
\alias{gstudy_nested_ip}
\title{Estimate Variance Components for a Nested Items-within-Person Design}
\usage{
gstudy_nested_ip(data, person, item, score)
}
\arguments{
  \item{data}{A data frame containing one row per observation.}
  \item{person}{Name of the person column.}
  \item{item}{Name of the nested item column.}
  \item{score}{Name of the numeric score column.}
}
\value{
An object of class \code{"gstudy_gtheoryr"}.
}
\description{
Estimates ANOVA mean squares and variance components for a simple balanced
nested design in which each person has their own set of items.
}
\examples{
nested_scores <- data.frame(
  person = c("P1", "P1", "P2", "P2", "P3", "P3"),
  item = c("P1_I1", "P1_I2", "P2_I1", "P2_I2", "P3_I1", "P3_I2"),
  score = c(8, 6, 5, 4, 9, 7)
)

gs_nested <- gstudy_nested_ip(
  nested_scores,
  person = "person",
  item = "item",
  score = "score"
)
gs_nested
}
