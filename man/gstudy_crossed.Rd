\name{gstudy_crossed}
\alias{gstudy_crossed}
\alias{dstudy_crossed}
\title{Generic Crossed G-study and D-study Helpers}
\usage{
gstudy_crossed(data, person, facets, score, facet_labels = facets)

dstudy_crossed(gstudy, design_levels = gstudy$design_levels)
}
\arguments{
  \item{data}{A data frame containing one row per observed cell.}
  \item{person}{Name of the person column.}
  \item{facets}{Character vector of facet column names.}
  \item{score}{Name of the numeric score column.}
  \item{facet_labels}{Optional user-facing labels for the supplied facets.}
  \item{gstudy}{A result from \code{gstudy_crossed()} or one of its crossed-design wrappers.}
  \item{design_levels}{Optional named numeric vector giving the facet counts to use in the D-study. Defaults to the current design in \code{gstudy}.}
}
\value{
For \code{gstudy_crossed()}, an object of class \code{"gstudy_gtheoryr"}.

For \code{dstudy_crossed()}, an object of class \code{"dstudy_gtheoryr"}.
}
\description{
\code{gstudy_crossed()} estimates ANOVA mean squares and variance components for
a balanced crossed random-effects design with persons as the object of
measurement and one or more additional facets.

\code{dstudy_crossed()} computes relative and absolute error variances, the
generalizability coefficient, and the phi coefficient for the current or
proposed levels of the additional facets.
}
\examples{
scores <- read.csv(
  system.file("extdata", "crossed_scores_rater_occasion.csv", package = "gtheoryr"),
  stringsAsFactors = FALSE
)

gs <- gstudy_crossed(
  scores,
  person = "person",
  facets = c("item", "rater", "occasion"),
  score = "score",
  facet_labels = c("item", "rater", "occasion")
)
gs
dstudy_crossed(gs)
}
