\name{dstudy_nested_ip}
\alias{dstudy_nested_ip}
\title{Design Study for a Nested Items-within-Person Design}
\usage{
dstudy_nested_ip(gstudy, n_items = gstudy$n_items)
}
\arguments{
  \item{gstudy}{A result from \code{gstudy_nested_ip()}.}
  \item{n_items}{Number of nested items per person in the proposed design.}
}
\value{
An object of class \code{"dstudy_gtheoryr"}.
}
\description{
Computes a simple reliability summary for a proposed number of nested items
per person.
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
dstudy_nested_ip(gs_nested, n_items = 4)
}
