# Internal helpers shared by the planning functions.
gt_require_design <- function(data, person, facets, score, design = "crossed") {
  report <- check_gstudy_design(data, person, facets, score, design)
  if (!report$valid) stop(paste(report$issues, collapse = " "), call. = FALSE)
}

gt_integer <- function(x, name, minimum = 1) {
  if (!is.numeric(x) || !length(x) || anyNA(x) ||
      any(!is.finite(x) | x < minimum | x != floor(x))) {
    stop(name, " must contain finite integers >= ", minimum, ".", call. = FALSE)
  }
}

gt_names <- function(x, name) {
  if (!is.character(x) || !length(x) || anyNA(x) ||
      any(!nzchar(x)) || anyDuplicated(x)) {
    stop(name, " must contain distinct, nonempty names.", call. = FALSE)
  }
}

gt_probability <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x <= 0 || x >= 1)
    stop(name, " must be strictly between zero and one.", call. = FALSE)
}

gt_model <- function(gstudy, design_levels = NULL, negative = c("error", "zero")) {
  negative <- match.arg(negative)
  if (!inherits(gstudy, "gstudy_gtheoryr"))
    stop("Supply a gstudy_gtheoryr object.", call. = FALSE)
  vc <- gstudy$variance_components
  if (any(!is.finite(vc))) stop("Variance estimates must be finite.", call. = FALSE)
  adjusted <- names(vc)[vc < 0]
  if (length(adjusted) && negative == "error")
    stop("Negative variance estimates: ", paste(adjusted, collapse = ", "),
         ". Review the model or explicitly use negative = 'zero'.", call. = FALSE)
  vc[vc < 0] <- 0
  if (identical(gstudy$design, "i:p")) {
    levels <- c(item = gstudy$n_items)
    factors <- list(person = "person", nested_item = c("person", "item"))
  } else if (!is.null(gstudy$component_factors)) {
    levels <- gstudy$design_levels
    factors <- gstudy$component_factors
  } else if (identical(gstudy$design, "p x i")) {
    levels <- c(item = gstudy$n_items)
    factors <- list(person = "person", item = "item", residual = c("person", "item"))
  } else if (identical(gstudy$design, "p x i x f")) {
    levels <- c(item = gstudy$n_items,
                stats::setNames(gstudy$n_facets, gstudy$facet_name))
    factors <- strsplit(names(vc), ":", fixed = TRUE)
    names(factors) <- names(vc)
    factors$residual <- c("person", names(levels))
  } else stop("Unsupported G-study design.", call. = FALSE)
  if (!is.null(design_levels)) {
    if (!is.numeric(design_levels)) stop("design_levels must be numeric.", call. = FALSE)
    gt_names(names(design_levels), "design_levels names")
    if (!setequal(names(levels), names(design_levels)))
      stop("design_levels must name every facet: ", paste(names(levels), collapse = ", "), call. = FALSE)
    levels <- design_levels[names(levels)]
  }
  gt_integer(levels, "design_levels")
  list(vc = vc, levels = levels, factors = factors, adjusted = adjusted)
}

gt_budget <- function(model) {
  vc <- model$vc
  den <- vapply(model$factors, function(f) prod(model$levels[setdiff(f, "person")]), numeric(1))
  person <- names(vc) == "person"
  rel <- vapply(model$factors, function(f) "person" %in% f, logical(1)) & !person
  data.frame(component = names(vc), estimate = unname(vc), divisor = unname(den),
             relative_contribution = unname(ifelse(rel, vc / den, 0)),
             absolute_contribution = unname(ifelse(!person, vc / den, 0)))
}

gt_metrics <- function(model) {
  b <- gt_budget(model)
  re <- sum(b$relative_contribution)
  ae <- sum(b$absolute_contribution)
  pv <- unname(model$vc["person"])
  c(relative_error = re, absolute_error = ae,
    g_coefficient = if (pv + re > 0) pv / (pv + re) else NA_real_,
    phi_coefficient = if (pv + ae > 0) pv / (pv + ae) else NA_real_,
    relative_sem = sqrt(re), absolute_sem = sqrt(ae))
}

#' Check Data Before Fitting a G-study
#'
#' Reports missing scores and identifiers, duplicate cells, missing crossed
#' cells and insufficient facet levels without fitting a model or dropping rows.
#' @param data A data frame in long format.
#' @param person Name of the person column.
#' @param facets Character vector of facet columns. For nested designs supply
#'   the single item column, with globally unique item labels.
#' @param score Name of the numeric score column.
#' @param design Either "crossed" or "nested" (items within persons).
#' @return A list with valid, issues, level_counts and diagnostic row/cell counts.
#'   Missing cells refer to the Cartesian product of observed nonmissing levels.
#' @examples
#' d <- data.frame(p = rep(1:3, each = 2), i = rep(1:2, 3), y = 1:6)
#' check_gstudy_design(d, "p", "i", "y")
#' @export
check_gstudy_design <- function(data, person, facets, score,
                                design = c("crossed", "nested")) {
  design <- match.arg(design)
  if (!is.data.frame(data)) stop("data must be a data frame.", call. = FALSE)
  person <- as_name(person)
  score <- as_name(score)
  gt_names(c(person, facets, score), "Column arguments")
  if (!length(facets)) stop("Supply at least one facet.", call. = FALSE)
  validate_columns(data, c(person, facets, score))
  if (design == "nested" && length(facets) != 1L)
    stop("Nested diagnostics support one item facet.", call. = FALSE)
  ids <- data[c(person, facets)]
  bad_ids <- !stats::complete.cases(ids)
  numeric_score <- is.numeric(data[[score]])
  bad_scores <- if (numeric_score) sum(!is.finite(data[[score]])) else nrow(data)
  levels <- vapply(ids, function(x) length(unique(x[!is.na(x)])), integer(1))
  observed <- ids[!bad_ids, , drop = FALSE]
  duplicates <- sum(duplicated(observed))
  missing_cells <- if (design == "crossed") prod(levels) - nrow(unique(observed)) else NA_real_
  issues <- character()
  if (!nrow(data)) issues <- c(issues, "No observations.")
  if (any(bad_ids)) issues <- c(issues, "Missing identifiers.")
  if (!numeric_score) issues <- c(issues, "Scores must be numeric.")
  if (bad_scores && numeric_score) issues <- c(issues, "Missing or nonfinite scores.")
  if (duplicates) issues <- c(issues, "Duplicate observation cells.")
  if (any(levels < 2)) issues <- c(issues, "Each factor needs at least two observed levels.")
  if (design == "crossed" && missing_cells > 0) issues <- c(issues, "Missing crossed cells.")
  if (design == "nested") {
    n <- table(as.character(observed[[person]]))
    if (!length(n) || length(unique(n)) != 1L || any(n < 2))
      issues <- c(issues, "Nested design needs equal counts of at least two items per person.")
    if (anyDuplicated(observed[[facets]])) issues <- c(issues, "Nested item labels must be globally unique.")
  }
  list(valid = !length(issues), issues = issues, level_counts = levels,
       n_rows = nrow(data), missing_id_rows = sum(bad_ids),
       invalid_score_rows = bad_scores, duplicate_rows = duplicates,
       missing_cells = missing_cells)
}

#' Decompose D-study Error by Variance Component
#' @param gstudy A fitted G-study from any supported estimator.
#' @param design_levels Optional named integer vector of proposed facet counts.
#'   Use singular names, for example c(item = 10, rater = 3).
#' @param negative Either "error" (default) or "zero". The latter explicitly
#'   truncates negative component estimates for planning and records their names
#'   in the adjusted_components attribute. It does not change the fitted object.
#' @return A data frame of component estimates, divisors, and contributions to
#'   relative and absolute error variance. All facets are random.
#' @examples
#' d <- data.frame(p = rep(1:3, each = 2), i = rep(1:2, 3), y = 1:6)
#' gs <- gstudy_pxi(d, "p", "i", "y")
#' error_budget(gs, negative = "zero")
#' @export
error_budget <- function(gstudy, design_levels = NULL, negative = c("error", "zero")) {
  m <- gt_model(gstudy, design_levels, match.arg(negative))
  out <- gt_budget(m)
  attr(out, "adjusted_components") <- m$adjusted
  out
}

#' Report Relative and Absolute Standard Errors of Measurement
#' @inheritParams error_budget
#' @return A two-row data frame with decision, error_variance and sem, on the
#'   mean-score scale. SEM is the square root of the corresponding D-study error
#'   variance, not the sampling standard error of a reliability coefficient.
#' @export
sem_gtheory <- function(gstudy, design_levels = NULL, negative = c("error", "zero")) {
  m <- gt_model(gstudy, design_levels, match.arg(negative))
  v <- gt_metrics(m)
  out <- data.frame(decision = c("relative", "absolute"),
                    error_variance = unname(v[1:2]), sem = unname(v[5:6]))
  attr(out, "adjusted_components") <- m$adjusted
  out
}

#' Construct Approximate Measurement Intervals Around Mean Scores
#' @inheritParams error_budget
#' @param scores Numeric vector of observed mean scores on the original scale.
#' @param decision "absolute" (default) or "relative".
#' @param conf Confidence level strictly between zero and one.
#' @return A data frame with score, sem, lower and upper. Normal-theory intervals
#'   use a common SEM and treat estimated variance components as known. They are
#'   not conditional SEMs, prediction intervals or coefficient confidence
#'   intervals. Relative intervals exclude systematic facet effects and should
#'   not be used for absolute cut-score decisions. Bounds are not clipped.
#' @export
score_interval <- function(gstudy, scores, design_levels = NULL,
                            decision = c("absolute", "relative"), conf = 0.95,
                            negative = c("error", "zero")) {
  decision <- match.arg(decision)
  gt_probability(conf, "conf")
  if (!is.numeric(scores) || !length(scores) || any(!is.finite(scores)))
    stop("scores must be a nonempty finite numeric vector.", call. = FALSE)
  s <- sem_gtheory(gstudy, design_levels, match.arg(negative))
  se <- s$sem[match(decision, s$decision)]
  margin <- stats::qnorm((1 + conf) / 2) * se
  out <- data.frame(score = scores, sem = se, lower = scores - margin, upper = scores + margin)
  attr(out, "decision") <- decision
  attr(out, "conf") <- conf
  attr(out, "adjusted_components") <- attr(s, "adjusted_components")
  out
}

#' Compare a Grid of Candidate D-study Designs
#' @inheritParams error_budget
#' @param candidates Named list of positive integer vectors, one for each facet.
#' @return A data frame with facet counts, observations per person, error
#'   variances, G and Phi coefficients and SEMs. At most 100000 combinations.
#'   Undefined coefficients (zero universe variance and zero error) are NA.
#' @export
dstudy_grid <- function(gstudy, candidates, negative = c("error", "zero")) {
  m <- gt_model(gstudy, negative = match.arg(negative))
  reserved <- c(names(gt_metrics(m)), "observations", "cost", "meets_target")
  if (any(names(m$levels) %in% reserved))
    stop("Facet labels conflict with output columns; refit with different facet_labels.", call. = FALSE)
  if (!is.list(candidates) || !length(candidates)) stop("candidates must be a named list.", call. = FALSE)
  gt_names(names(candidates), "candidates names")
  if (!setequal(names(candidates), names(m$levels))) stop("candidates must name every facet.", call. = FALSE)
  candidates <- lapply(candidates[names(m$levels)], unique)
  for (nm in names(candidates)) gt_integer(candidates[[nm]], nm)
  if (prod(lengths(candidates)) > 100000) stop("Grid exceeds 100000 designs.", call. = FALSE)
  out <- expand.grid(candidates, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  values <- t(vapply(seq_len(nrow(out)), function(j) {
    m$levels <- unlist(out[j, , drop = FALSE], use.names = TRUE)
    gt_metrics(m)
  }, numeric(6)))
  out$observations <- apply(out, 1, prod)
  out <- cbind(out, as.data.frame(values))
  attr(out, "adjusted_components") <- m$adjusted
  out
}

#' Find the Least Cost Candidate Meeting a Reliability Target
#' @inheritParams dstudy_grid
#' @param target Required reliability strictly between zero and one.
#' @param coefficient Either "phi" or "g".
#' @param costs Optional nonnegative named per-level costs for every facet.
#' @param observation_cost Nonnegative cost per observed cell per person.
#' @param budget Maximum allowed cost; defaults to Inf.
#' @return A list with feasible, best (all minimum-cost ties), evaluated and
#'   target. Cost equals sum(costs * counts) + observation_cost * prod(counts),
#'   expressed per person. Without costs, cost is the number of observations.
#'   The optimum is only over the supplied candidate grid. No feasible design
#'   returns a zero-row best table rather than an invented recommendation.
#' @references Meyer, Liu and Mashburn (2014), doi:10.1177/0013164413508774.
#' @export
optimize_dstudy <- function(gstudy, candidates, target = 0.8,
                             coefficient = c("phi", "g"), costs = NULL,
                             observation_cost = 1, budget = Inf,
                             negative = c("error", "zero")) {
  coefficient <- match.arg(coefficient)
  gt_probability(target, "target")
  if (!is.numeric(observation_cost) || length(observation_cost) != 1L ||
      !is.finite(observation_cost) || observation_cost < 0)
    stop("observation_cost must be finite and nonnegative.", call. = FALSE)
  if (!is.numeric(budget) || length(budget) != 1L || is.na(budget) || budget < 0)
    stop("budget must be nonnegative.", call. = FALSE)
  tab <- dstudy_grid(gstudy, candidates, match.arg(negative))
  facets <- names(candidates)
  if (is.null(costs)) costs <- stats::setNames(rep(0, length(facets)), facets)
  gt_names(names(costs), "costs names")
  if (!is.numeric(costs) || !setequal(names(costs), facets) ||
      any(!is.finite(costs) | costs < 0)) stop("Supply finite nonnegative costs for every facet.", call. = FALSE)
  tab$cost <- as.vector(as.matrix(tab[facets]) %*% costs[facets]) + observation_cost * tab$observations
  criterion <- tab[[paste0(coefficient, "_coefficient")]]
  tab$meets_target <- is.finite(criterion) & criterion >= target & tab$cost <= budget
  best <- tab[tab$meets_target, , drop = FALSE]
  if (nrow(best)) best <- best[best$cost == min(best$cost), , drop = FALSE]
  list(feasible = nrow(best) > 0, best = best, evaluated = tab, target = target,
       coefficient = coefficient, adjusted_components = attr(tab, "adjusted_components"))
}

#' Examine Gains from Increasing One Facet at a Time
#' @inheritParams error_budget
#' @param increment Positive integer increase applied separately to each facet.
#' @return One row per facet with old and new counts, added observations per
#'   person, new coefficients and changes from the baseline G and Phi. This is
#'   a local comparison conditional on the fitted variance components.
#' @export
dstudy_sensitivity <- function(gstudy, design_levels = NULL, increment = 1,
                                negative = c("error", "zero")) {
  gt_integer(increment, "increment")
  if (length(increment) != 1L) stop("increment must be scalar.", call. = FALSE)
  m <- gt_model(gstudy, design_levels, match.arg(negative))
  base <- gt_metrics(m)
  out <- do.call(rbind, lapply(names(m$levels), function(nm) {
    old <- m$levels[[nm]]
    n0 <- prod(m$levels)
    m$levels[[nm]] <- old + increment
    v <- gt_metrics(m)
    data.frame(facet = nm, old_count = old, new_count = old + increment,
      added_observations = prod(m$levels) - n0,
      g_coefficient = unname(v[3]), phi_coefficient = unname(v[4]),
      delta_g = unname(v[3] - base[3]), delta_phi = unname(v[4] - base[4]))
  }))
  attr(out, "adjusted_components") <- m$adjusted
  out
}

gt_seed <- function(seed, code) {
  if (is.null(seed)) return(force(code))
  gt_integer(seed, "seed", 0)
  if (length(seed) != 1L || seed > .Machine$integer.max) stop("Invalid seed.", call. = FALSE)
  exists_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (exists_seed) previous <- get(".Random.seed", envir = .GlobalEnv)
  on.exit(if (exists_seed) assign(".Random.seed", previous, envir = .GlobalEnv)
          else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
            rm(".Random.seed", envir = .GlobalEnv), add = TRUE)
  set.seed(seed)
  force(code)
}

#' Simulate a Balanced Gaussian Crossed G-study
#' @param n_persons Number of persons, an integer at least two.
#' @param design_levels Named integer counts of at least two for each facet.
#' @param variance_components Named nonnegative variances. Names use person,
#'   facet names and colon-separated interactions in design order. The highest
#'   interaction is named residual. Omitted components are zero.
#' @param mean Grand mean of the simulated scores.
#' @param seed Optional integer seed, restoring the caller's RNG state afterward.
#' @return A long-format data frame with person, facet columns and score.
#'   Independent Gaussian effects are shared by observations with the same
#'   factor combination. The highest interaction and cell error are confounded.
#'   Scores are continuous and unbounded; no ordinal or binary model is implied.
#' @examples
#' d <- simulate_gstudy(20, c(item = 4),
#'                      c(person = 2, item = 0.2, residual = 1), seed = 42)
#' gstudy_crossed(d, "person", "item", "score")
#' @export
simulate_gstudy <- function(n_persons, design_levels,
                             variance_components, mean = 0, seed = NULL) {
  gt_integer(n_persons, "n_persons", 2)
  if (length(n_persons) != 1L) stop("n_persons must be scalar.", call. = FALSE)
  gt_names(names(design_levels), "design_levels names")
  gt_integer(design_levels, "design_levels", 2)
  labels <- names(design_levels)
  if (any(labels %in% c("person", "score", "residual")) || any(grepl(":", labels, fixed = TRUE)))
    stop("Facet names cannot be person, score, residual or contain colons.", call. = FALSE)
  if (length(labels) > 7L || n_persons * prod(design_levels) > 1e6)
    stop("Simulation limited to seven facets and one million cells.", call. = FALSE)
  parts <- all_factor_subsets(c("person", labels))
  allowed <- vapply(parts, effect_name, character(1))
  allowed[length(allowed)] <- "residual"
  gt_names(names(variance_components), "variance_components names")
  if (!is.numeric(variance_components) || any(!is.finite(variance_components) | variance_components < 0) ||
      any(!names(variance_components) %in% allowed))
    stop("Invalid variance component names or values.", call. = FALSE)
  if (!is.numeric(mean) || length(mean) != 1L || !is.finite(mean)) stop("mean must be finite.", call. = FALSE)
  gt_seed(seed, {
    counts <- c(person = n_persons, design_levels)
    d <- expand.grid(lapply(counts, seq_len), KEEP.OUT.ATTRS = FALSE)
    y <- rep(mean, nrow(d))
    for (nm in names(variance_components)) {
      if (variance_components[[nm]] == 0) next
      f <- parts[[match(nm, allowed)]]
      key <- do.call(interaction, c(d[f], list(drop = TRUE, lex.order = TRUE)))
      y <- y + stats::rnorm(nlevels(key), sd = sqrt(variance_components[[nm]]))[as.integer(key)]
    }
    d$score <- y
    d
  })
}

#' Parametric Bootstrap Uncertainty for a Crossed G-study
#' @inheritParams error_budget
#' @param B Number of bootstrap replicates, an integer at least two. Use at least
#'   1000 for substantive work and inspect Monte Carlo stability.
#' @param conf Confidence level strictly between zero and one.
#' @param seed Optional integer seed, restoring the caller's RNG state afterward.
#' @return A list with percentile intervals (estimate, lower, upper, n_valid),
#'   replicate estimates, B, conf, method, adjusted_components and
#'   replicate_adjustments. Simulates all random facets at their original sample
#'   sizes, then refits ANOVA. Proposed design_levels affect D-study results only.
#'   Refit negative components are truncated to zero and counted. Raw variance
#'   estimates are retained in replicate output; coefficient estimates use the
#'   truncated components. Undefined coefficients remain NA and are counted.
#'   This Gaussian parametric method is not the nonparametric bias-corrected
#'   procedure described by Tong and Brennan. Coverage is approximate, especially
#'   near zero variance boundaries. Simple nested designs are not supported.
#' @references Tong and Brennan (2007), doi:10.1177/0013164407301533.
#' @export
bootstrap_gstudy <- function(gstudy, B = 1000, design_levels = NULL, conf = 0.95,
                              seed = NULL, negative = c("error", "zero")) {
  gt_integer(B, "B", 2)
  if (length(B) != 1L) stop("B must be scalar.", call. = FALSE)
  gt_probability(conf, "conf")
  negative <- match.arg(negative)
  m <- gt_model(gstudy, design_levels, negative)
  original <- gt_model(gstudy, negative = negative)
  if (identical(gstudy$design, "i:p")) stop("Bootstrap currently supports crossed designs only.", call. = FALSE)
  point <- c(stats::setNames(gstudy$variance_components, paste0("vc_", names(m$vc))), gt_metrics(m))
  result <- gt_seed(seed, {
    adjusted <- integer(B)
    draws <- t(vapply(seq_len(B), function(b) {
      d <- simulate_gstudy(gstudy$n_persons, original$levels, original$vc)
      fit <- gstudy_crossed(d, "person", names(original$levels), "score")
      refit <- gt_model(fit, m$levels, "zero")
      adjusted[b] <<- length(refit$adjusted)
      c(unname(fit$variance_components[names(m$vc)]), gt_metrics(refit))
    }, numeric(length(point))))
    list(draws = draws, adjusted = adjusted)
  })
  colnames(result$draws) <- names(point)
  valid <- colSums(is.finite(result$draws))
  bounds <- vapply(seq_along(point), function(j) {
    x <- result$draws[, j]
    x <- x[is.finite(x)]
    if (length(x) < 2) return(c(NA_real_, NA_real_))
    unname(stats::quantile(x, c((1 - conf) / 2, (1 + conf) / 2)))
  }, numeric(2))
  if (any(valid < B)) warning("Some bootstrap quantities are undefined; inspect n_valid.", call. = FALSE)
  list(intervals = data.frame(quantity = names(point), estimate = unname(point),
         lower = bounds[1, ], upper = bounds[2, ], n_valid = unname(valid), row.names = NULL),
       replicates = as.data.frame(result$draws), B = B, conf = conf,
       method = "Gaussian parametric percentile bootstrap",
       adjusted_components = m$adjusted, replicate_adjustments = result$adjusted)
}
