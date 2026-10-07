# gtheoryr 0.2.0

Nine new functions extend the analysis and planning workflow:

* `check_gstudy_design()` diagnoses invalid scores, missing IDs, duplicate cells,
  missing crossed cells, insufficient levels and simple nested imbalance.
* `error_budget()` shows each component's contribution to D-study error.
* `sem_gtheory()` reports relative and absolute standard errors of measurement.
* `score_interval()` constructs approximate normal measurement intervals.
* `dstudy_grid()` compares candidate facet counts in a single table.
* `optimize_dstudy()` finds minimum-cost candidates meeting a G or Phi target.
* `dstudy_sensitivity()` compares gains from increasing each facet separately.
* `simulate_gstudy()` generates balanced Gaussian crossed data.
* `bootstrap_gstudy()` estimates Gaussian parametric percentile intervals for
  crossed-design variance components and D-study quantities.

New planning functions reject negative variance estimates unless the caller
explicitly requests truncation, and record adjusted components. Existing
D-study functions retain their original raw-estimate behavior.

Fixed numeric ID indexing in persons-by-items and nested estimators, handled
unused factor levels, added validation to all G-study entry points, and rejected
ambiguous facet labels. ANOVA extraction now uses the aov summary rather than
requesting F-tests from a saturated model. No new package dependencies.

See `inst/doc/planning-guide.md` for assumptions, sources and a worked example.
