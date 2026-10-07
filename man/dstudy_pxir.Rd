\name{dstudy_pxir}
\alias{dstudy_pxir}
\alias{dstudy_pxio}
\title{D-study Wrappers for Common Three-Facet Designs}
\usage{
dstudy_pxir(gstudy, n_items = gstudy$n_items, n_raters = gstudy$n_facets)

dstudy_pxio(gstudy, n_items = gstudy$n_items, n_occasions = gstudy$n_facets)
}
\arguments{
  \item{gstudy}{A result from \code{gstudy_pxir()} or \code{gstudy_pxio()}.}
  \item{n_items}{Number of items in the design. Defaults to the current number in the supplied \code{gstudy} object.}
  \item{n_raters}{Number of raters in the design. Defaults to the current number in the supplied \code{gstudy} object.}
  \item{n_occasions}{Number of occasions in the design. Defaults to the current number in the supplied \code{gstudy} object.}
}
\value{
An object of class \code{"dstudy_gtheoryr"}.
}
\description{
Convenience wrappers around \code{dstudy_pxif()} for crossed persons-by-items-by-raters
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

gs_rater <- gstudy_pxir(
  scores_rater,
  person = "person",
  item = "item",
  rater = "rater",
  score = "score"
)
dstudy_pxir(gs_rater)

scores_occasion <- expand.grid(
  person = c("P1", "P2", "P3"),
  item = c("I1", "I2"),
  occasion = c("T1", "T2"),
  stringsAsFactors = FALSE
)
scores_occasion$score <- c(8, 8, 7, 7, 5, 5, 6, 6, 7, 7, 8, 8)

gs_occasion <- gstudy_pxio(
  scores_occasion,
  person = "person",
  item = "item",
  occasion = "occasion",
  score = "score"
)
dstudy_pxio(gs_occasion)
}
