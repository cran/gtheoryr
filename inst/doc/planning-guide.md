# Extending gtheoryr for practical assessment planning
Version 0.2.0 | Research and update guide | 7 October 2026

The most useful next step for gtheoryr is to help someone decide what to do after fitting a G-study. A table of variance components can tell us where scores vary. The next questions are usually more practical: how uncertain is a score, would another rater help, and what is the least expensive design that meets our needs?

This update adds nine functions around those questions. It keeps the package focused on balanced random-effects designs and adds no package dependencies. The original estimation and D-study functions remain available.

## What the research suggests
Briesch and colleagues (2014) describe study design and interpretation as central parts of a useful generalizability analysis. That supports a workflow which checks the data before estimation and makes the intended decision explicit [1].

Brennan (2003) distinguishes relative error, which matters for comparisons among people, from absolute error, which also includes systematic differences between measurement conditions. Their square roots give the corresponding standard errors of measurement [2].

Meyer, Liu and Mashburn (2014) address the practical problem of improving reliability within a budget. Their work motivates including costs when choosing facet counts. The new optimizer uses an explicit search over integer candidates, rather than reproducing their Lagrange multiplier solution [3].

Tong and Brennan examine bootstrap uncertainty in G-theory and show why resampling must respect the design. Their work motivates reporting uncertainty, but the new bootstrap uses a Gaussian parametric model; it is not their bias-corrected nonparametric procedure [4, 5].

## The nine additions
1. check_gstudy_design checks whether the input fits the supported design.
2. error_budget explains which components contribute to measurement error.
3. sem_gtheory reports relative and absolute SEMs.
4. score_interval places approximate measurement intervals around mean scores.
5. dstudy_grid compares candidate numbers of items, raters or other facets.
6. optimize_dstudy finds the cheapest candidates meeting a chosen target.
7. dstudy_sensitivity compares the gain from increasing each facet separately.
8. simulate_gstudy generates reproducible continuous crossed-design data.
9. bootstrap_gstudy estimates uncertainty for crossed-design results.


# Checking data and explaining error
## Check the design before estimating it
check_gstudy_design(data, person, facets, score) reports missing identifiers, nonnumeric or nonfinite scores, repeated observation cells, missing crossed cells and factors with too few observed levels. It returns a valid flag and a list of issues, so a script can stop with a useful explanation.

For the simple nested design, use design = "nested" and supply one item column. Each person must have the same number of items, with at least two items per person. Item labels must be unique across the dataset, matching the existing estimator's convention.

The diagnostic cannot detect a person or item that is absent from the dataset altogether. Its expected crossed table is based on the levels actually observed. It reports problems without imputing scores or averaging duplicates. These checks also run inside the existing G-study estimators.

## Show where the error comes from
error_budget(gs, design_levels) returns a row for each variance component. The divisor shows how a proposed design reduces that component; the final columns show its contribution to relative and absolute error. design_levels uses singular facet labels, such as c(item = 10, rater = 3).

For a persons-by-items design with person variance 4, item variance 1 and residual variance 6, three items give relative error 6/3 = 2 and absolute error (1 + 6)/3 = 2.3333. The corresponding G coefficient is 4/(4 + 2) = 0.6667, and Phi is 4/(4 + 2.3333) = 0.6316. This is a hand-worked example, not an empirical result.

## Put error on the score scale
sem_gtheory(gs) takes the square roots of the two error variances. In the example above, relative SEM is 1.4142 and absolute SEM is 1.5275. These values describe measurement error in an individual's mean score. They do not describe the sampling uncertainty of an estimated reliability coefficient.

score_interval(gs, scores = 50, design_levels = c(item = 3)) uses the absolute SEM by default. With the example components, the approximate 95 percent interval is 47.01 to 52.99. The calculation is score plus or minus 1.96 times SEM.

These intervals assume approximately normal, constant measurement error and treat estimated components as known. They are not conditional intervals tailored to each person's response pattern. Supply mean scores, not totals. Relative intervals omit systematic facet effects and should not be used to make absolute cut-score decisions. Bounds are left on the original scale without clipping.


# Choosing a workable assessment design
## Compare candidate designs
dstudy_grid(gs, list(item = 4:12, rater = 2:5)) evaluates every combination in the supplied ranges. It returns facet counts, observations per person, both error variances, G, Phi and both SEMs. This makes the trade-off between longer tests and additional raters visible in one table.

All facets are treated as random, and the component estimates stay fixed while the proposed counts change. A design projection assumes that adding items or raters does not change the underlying variance structure. That assumption deserves attention if the added items are much harder or the new raters receive different training.

## Include costs in the decision
optimize_dstudy searches the same grid and returns every minimum-cost candidate that meets the chosen reliability target and budget. It defaults to Phi and a target of 0.80. That default is a starting value for software use; it is not a universal standard for acceptable reliability.

The cost model is expressed per person:

    cost = sum(per_level_cost * facet_count)
           + observation_cost * product(facet_counts)

For example, costs = c(item = 1, rater = 5) and observation_cost = 0.5 make ten items with three raters cost 10 + 15 + 15 = 40 units per person. The user chooses the units. This simple formula does not model shared setup costs or complicated staffing arrangements.

For the hand-worked persons-by-items example on the previous page, a Phi target of 0.80 needs at least seven items: 4/(4 + 7/7) = 0.80. Searching items 1 through 10 with the default observation cost selects seven. A search restricted to items 1 through 5 returns feasible = FALSE and an empty best table.

The result is optimal only among the supplied candidates. If it recommends the largest value in the grid, consider extending the range. Check best and evaluated rather than assuming that a feasible design always exists.

## See which extra observation would help
dstudy_sensitivity(gs) increases each facet by one in turn and holds the others fixed. It reports the new coefficients, changes from the current design and the number of extra observations per person. increment can specify a larger increase.

This is useful when comparing one more item with one more rater. The larger reliability gain may also require more scoring time, so read the gain alongside added_observations and the cost results. This is a local comparison, not a search for the best joint redesign.


# Simulation and uncertainty
## Generate data with known components
simulate_gstudy creates a complete crossed table and draws independent Gaussian effects for every supplied component. Observations sharing a person, item or interaction level share the corresponding effect. Components omitted from the named vector are set to zero.

The function accepts any supported crossed combination up to seven facets and one million cells. Each simulated factor needs at least two levels. Interaction names follow the order of design_levels; the highest interaction is called residual because it cannot be separated from cell error with one observation per cell.

Use this for teaching, checking analysis scripts and studying how estimates vary across repeated samples. The scores are continuous and unbounded. Rounding them into a rating scale changes the generating model and its variance components. A supplied seed makes the result reproducible and restores the caller's random-number state afterward.

## Quantify uncertainty in the fitted results
bootstrap_gstudy(gs, B = 1000, seed = 123) generates new crossed datasets at the original G-study sample sizes, refits the ANOVA model and calculates the requested D-study quantities. It returns percentile intervals for variance components, G, Phi, error variances and SEMs, along with every replicate estimate.

If design_levels specifies a future assessment, it changes the D-study calculations within each replicate. It does not pretend that the original G-study collected more observations. A grand mean of zero is sufficient for these simulations because the reported quantities are invariant to adding a constant to every score.

Start with a small B while developing a script. For substantive analysis, use at least 1000 replicates and check whether the interval endpoints remain reasonably stable with more replicates or another seed. Small samples and variance estimates near zero can make percentile intervals unstable. Coverage has not been established for every design supported by the package.

## Be explicit about negative estimates
ANOVA can produce negative variance estimates. The new planning functions stop by default when this occurs. Setting negative = "zero" requests truncation for planning and records the adjusted component names. The original fitted object is preserved.

The bootstrap generates data from nonnegative components. Its refits retain raw component estimates in the replicate table, but truncate negative estimates when calculating reliability and SEM. replicate_adjustments records how many components were truncated in each replicate. n_valid reports how many finite values contributed to each interval; undefined coefficients remain missing.

The bootstrap currently supports crossed designs only. Fixed facets, unbalanced or incomplete designs, multivariate G-theory, binary response models and classification consistency need separate statistical work. This release does not claim to handle them.


# A short workflow to try
The following example uses simulated scores. It is also saved as inst/examples/planning.R in the package, where it includes the remaining new functions. Run it after installing version 0.2.0.

```r
library(gtheoryr)
d <- simulate_gstudy(
  30, c(item = 6, rater = 3),
  c(person = 4, item = 0.4, rater = 0.2,
    "person:item" = 0.8, "person:rater" = 0.3,
    "item:rater" = 0.1, residual = 1.5),
  mean = 50, seed = 2026
)
check_gstudy_design(
  d, "person", c("item", "rater"), "score"
)
gs <- gstudy_crossed(
  d, "person", c("item", "rater"), "score"
)
variance_components_table(gs)
eb <- error_budget(gs, negative = "zero")
attr(eb, "adjusted_components")
sem_gtheory(gs, negative = "zero")
plan <- optimize_dstudy(
  gs, list(item = 4:12, rater = 2:5),
  target = 0.85, coefficient = "phi",
  costs = c(item = 1, rater = 5),
  observation_cost = 0.5, negative = "zero"
)
plan$best
dstudy_sensitivity(gs, negative = "zero")
boot <- bootstrap_gstudy(
  gs, B = 1000, seed = 123, negative = "zero"
)
boot$intervals
```

The explicit truncation option keeps the example runnable if the simulated sample yields negative estimates. In an applied analysis, inspect those estimates first and report the choice to truncate. A coefficient alone does not establish that the assessment supports its intended interpretation.

## Existing behavior and repairs
The update corrects lookup errors when person or item IDs are numeric values such as 10 and 20, handles unused factor levels, rejects ambiguous facet labels and checks missing or invalid data before fitting. Existing D-study functions still use raw ANOVA components; the stricter negative-estimate policy belongs to the new functions.

Tests cover hand calculations, optimizer feasibility, numeric IDs, nested and multiple-facet compatibility, simulated component recovery, reproducibility and bootstrap output. See the accompanying validation file for the final package-check result and environment.


# Installing and maintaining the update
## Install the prepared source archive
The package source has been updated in C:/Users/Ujjwa/Documents/Playground/gtheoryr. The deliverable archive is gtheoryr_0.2.0.tar.gz. Restart R before installing so the old namespace is not still loaded.

```r
archive <- paste0(
  "C:/Users/Ujjwa/Documents/Codex/2026-10-02/okm/",
  "outputs/gtheoryr_0.2.0.tar.gz"
)
install.packages(archive, repos = NULL, type = "source")
library(gtheoryr)
packageVersion("gtheoryr")
help("optimize_dstudy", package = "gtheoryr")
```

packageVersion should report 0.2.0. The package itself contains no compiled code. Installing this archive updates your local R library; it does not publish a release to CRAN or GitHub. The update was tested in an isolated library so your normal installed package was not replaced during development.

## Rebuild after later changes
Edit the source folder, increment Version in DESCRIPTION and describe the change in NEWS.md. From a terminal with R on PATH, build the source archive and run the package checks. On this machine the executable is C:/Program Files/R/R-4.5.2/bin/R.exe.

    R CMD build C:/Users/Ujjwa/Documents/Playground/gtheoryr
    R CMD check --as-cran gtheoryr_0.2.0.tar.gz

Use the new version number in future filenames. A full check includes PDF manual generation and needs an appropriate TeX setup; --no-manual skips that step during local development. Run full release checks in a suitable environment before submitting.

New help pages are generated from roxygen comments in R/planning.R. The older package uses manually maintained Rd files, some combining several functions. Running roxygen over the whole package can create duplicate aliases; preserve the combined legacy help pages or migrate them deliberately. NAMESPACE is also maintained explicitly in this release.

## If you want to publish
Verify the maintainer email and package metadata, run checks on the current R release and another platform, and update cran-comments.md with actual results. For CRAN, submit the checked source archive through the CRAN submission form and complete the maintainer confirmation. If you distribute through GitHub, commit the package changes to its repository, push them and tag the release. No public submission or push was made as part of this update.

Keep the prior release until your own analysis scripts run successfully against the new one. A backup of the original source was made before editing.


# Articles and methodological sources
The source notes below explain what informed the update and where the implementation makes its own choices. Links lead to the article, publisher record or university-hosted report. The implementation should be cited separately from these methodological sources.

## Practical use of generalizability theory
[1] Briesch, A. M., Swaminathan, H., Welsh, M., and Chafouleas, S. M. (2014). Generalizability theory: A practical guide to study design, implementation, and interpretation. Journal of School Psychology, 52(1), 13-35.
https://doi.org/10.1016/j.jsp.2013.11.008

The accessible abstract and publisher record informed the emphasis on design and interpretation. The data-checking interface and function choices are package design decisions.

## Error variances and coefficients
[2] Brennan, R. L. (2003). Coefficients and Indices in Generalizability Theory. CASMA Research Report 1, University of Iowa. See especially the discussion of relative and absolute error on printed pages 3-4.
https://education.uiowa.edu/casma/pubs

The university-hosted report supplies the distinction between error types and the G and Phi ratios. The score-interval function adds an explicitly stated normal-error approximation; it does not implement the report's broader collection of indices.

## Designs under a budget
[3] Meyer, J. P., Liu, X., and Mashburn, A. J. (2014). A practical solution to optimizing the reliability of teaching observation measures under budget constraints. Educational and Psychological Measurement, 74(2), 280-291.
https://doi.org/10.1177/0013164413508774

The publisher's abstract describes optimization under cost constraints. The package implements a simpler transparent grid search with user-specified costs. It does not reproduce the article's closed-form optimization equations.

## Bootstrap uncertainty
[4] Tong, Y., and Brennan, R. L. (2006). Bootstrap Techniques for Estimating Variability in Generalizability Theory. CASMA Research Report 15, University of Iowa.
https://education.uiowa.edu/casma/pubs

[5] Tong, Y., and Brennan, R. L. (2007). Bootstrap estimates of standard errors in generalizability theory. Educational and Psychological Measurement.
https://doi.org/10.1177/0013164407301533

The report and article concern design-sensitive bootstrap procedures. They support taking estimation uncertainty seriously. The new function instead simulates independent Gaussian random effects and uses percentile limits, with its assumptions and boundary limitations documented in the help page.
