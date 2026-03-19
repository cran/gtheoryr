# gtheoryr

`gtheoryr` is a small R package for simple generalizability theory workflows.
It is intentionally modest in scope so it is easy to understand, extend, and
prepare for a first CRAN submission.

The package currently includes:

- `gstudy_pxi()` for a fully crossed persons-by-items design
- `gstudy_nested_ip()` for a simple balanced nested items-within-person design
- `dstudy_pxi()` for relative and absolute decision summaries
- `dstudy_nested_ip()` for a simple nested-design D-study

## Install locally

```r
install.packages("gtheoryr", repos = NULL, type = "source")
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

## CRAN readiness notes

Before submitting to CRAN, you should:

1. Verify that the maintainer details in `DESCRIPTION` are correct.
2. Run `R CMD check --as-cran gtheoryr`.
3. Add a `cran-comments.md` file summarizing check results.
4. Consider adding tests and a vignette once the API settles down.
