\name{anova_table}
\alias{anova_table}
\alias{anova_table.gstudy_gtheoryr}
\alias{mean_squares_table}
\alias{mean_squares_table.gstudy_gtheoryr}
\alias{variance_components_table}
\alias{variance_components_table.gstudy_gtheoryr}
\title{Extract Tables from a G-study Object}
\usage{
anova_table(x, ...)

mean_squares_table(x, ...)

variance_components_table(x, ...)
}
\arguments{
  \item{x}{A \code{"gstudy_gtheoryr"} object.}
  \item{...}{Unused.}
}
\value{
A data frame.
}
\description{
Convenience extractors for sums of squares, mean squares, and variance
components from a \code{"gstudy_gtheoryr"} object.
}
\examples{
scores <- data.frame(
  person = rep(c("P1", "P2", "P3"), each = 3),
  item = rep(c("I1", "I2", "I3"), times = 3),
  score = c(8, 7, 9, 5, 4, 6, 7, 6, 8)
)

gs <- gstudy_pxi(scores, person = "person", item = "item", score = "score")

anova_table(gs)
mean_squares_table(gs)
variance_components_table(gs)
}
