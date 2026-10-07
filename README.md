# gtheoryr

`gtheoryr` is a small R package for simple generalizability theory workflows.
It is intentionally modest in scope so it is easy to understand, extend, and
prepare for a first CRAN submission.

## New in version 0.2.0

Nine functions now connect the G-study to assessment planning:

| Function | Use |
| --- | --- |
| `check_gstudy_design()` | Diagnose missing scores, identifiers, cells and imbalance |
| `error_budget()` | Decompose relative and absolute D-study error |
| `sem_gtheory()` | Report measurement error on the mean-score scale |
| `score_interval()` | Construct approximate normal measurement intervals |
| `dstudy_grid()` | Compare a grid of candidate facet counts |
| `optimize_dstudy()` | Find the cheapest candidates meeting a G or Phi target |
| `dstudy_sensitivity()` | Compare gains from increasing each facet separately |
| `simulate_gstudy()` | Generate balanced continuous Gaussian crossed data |
| `bootstrap_gstudy()` | Estimate parametric bootstrap uncertainty for crossed designs |

Read [the planning guide](inst/doc/planning-guide.md) for the research sources,
assumptions and worked calculations. A runnable example is in
[inst/examples/planning.R](inst/examples/planning.R).

The new planning functions reject negative variance estimates unless you
explicitly request `negative = "zero"`; adjusted components are reported.
All facets are random. Designs must be balanced, with one observation per cell.
The bootstrap supports crossed designs and assumes independent Gaussian effects.
No additional package dependencies are required.

The package currently includes:

- `gstudy_pxi()` for a fully crossed persons-by-items design
- `gstudy_crossed()` for generic balanced crossed designs with one or more facets
- `gstudy_pxif()` for a fully crossed persons-by-items-by-facet design
- `gstudy_pxir()` and `gstudy_pxio()` as convenience wrappers for raters and occasions
- `gstudy_pxiro()` as a convenience wrapper for persons-by-items-by-raters-by-occasions designs
- `gstudy_nested_ip()` for a simple balanced nested items-within-person design
- `dstudy_pxi()` for relative and absolute decision summaries
- `dstudy_crossed()` for generic crossed-design D-studies
- `dstudy_pxif()` for current-design or proposed-design summaries with a third facet
- `dstudy_pxir()` and `dstudy_pxio()` as convenience wrappers for raters and occasions
- `dstudy_pxiro()` for current-design or proposed-design summaries with raters and occasions together
- `dstudy_nested_ip()` for a simple nested-design D-study
- `anova_table()`, `mean_squares_table()`, and `variance_components_table()`
  for pulling tidy output tables from a G-study object
- `variance_proportions_table()` for showing how much each variance component contributes

## Install locally

```r
install.packages("path/to/gtheoryr_0.2.0.tar.gz", repos = NULL, type = "source")
```

## Quick example

```r
library(gtheoryr)

scores <- data.frame(
  person = rep(c("P1", "P2", "P3"), each = 3),
  item = rep(c("I1", "I2", "I3"), times = 3),
  score = c(8, 7, 9, 5, 4, 6, 7, 6, 8)
)

gs <- gstudy_pxi(scores, person = "person", item = "item", score = "score")
gs

dstudy_pxi(gs, n_items = 6)
```

## Three-facet example

```r
library(gtheoryr)

scores <- expand.grid(
  person = c("P1", "P2", "P3"),
  item = c("I1", "I2"),
  rater = c("R1", "R2"),
  stringsAsFactors = FALSE
)

scores$score <- c(8, 7, 7, 6, 5, 4, 6, 5, 7, 6, 8, 7)

gs3 <- gstudy_pxif(
  scores,
  person = "person",
  item = "item",
  facet = "rater",
  score = "score",
  facet_name = "rater"
)

gs3
variance_components_table(gs3)
dstudy_pxif(gs3)
```

## Four-facet example

```r
library(gtheoryr)

scores4 <- read.csv(
  system.file("extdata", "crossed_scores_rater_occasion.csv", package = "gtheoryr"),
  stringsAsFactors = FALSE
)

gs4 <- gstudy_pxiro(
  scores4,
  person = "person",
  item = "item",
  rater = "rater",
  occasion = "occasion",
  score = "score"
)

gs4
variance_components_table(gs4)
variance_proportions_table(gs4)
dstudy_pxiro(gs4)
```

## CRAN readiness notes

Before submitting to CRAN, you should:

1. Verify that the maintainer details in `DESCRIPTION` are correct.
2. Run `R CMD check --as-cran gtheoryr`.
3. Add a `cran-comments.md` file summarizing check results.
4. Add tests and a vignette once the API settles down further.
