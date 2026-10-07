fixture_gt <- function() {
  gs <- gstudy_pxi(data.frame(p = rep(1:3, each = 2), i = rep(1:2, 3), y = 1:6), "p", "i", "y")
  gs$variance_components <- c(person = 4, item = 1, residual = 6)
  gs
}

test_that("diagnostics distinguish missing scores, cells and duplicates", {
  d <- data.frame(p = rep(c(10, 20, 30), each = 2), i = rep(c(100, 200), 3), y = 1:6)
  expect_true(check_gstudy_design(d, "p", "i", "y")$valid)
  expect_equal(check_gstudy_design(d[-1, ], "p", "i", "y")$missing_cells, 1)
  expect_equal(check_gstudy_design(rbind(d, d[1, ]), "p", "i", "y")$duplicate_rows, 1)
  d$y[1] <- NA_real_
  expect_false(check_gstudy_design(d, "p", "i", "y")$valid)
  expect_error(gstudy_pxi(d, "p", "i", "y"), "nonfinite")
  expect_error(gstudy_crossed(d, "p", "i", "y"), "nonfinite")
  expect_error(check_gstudy_design(d, "p", "p", "y"), "distinct")
})

test_that("numeric IDs and unused factor levels do not corrupt ANOVA", {
  d <- data.frame(p = rep(c(10, 20, 30), each = 2), i = rep(c(100, 200), 3), y = c(1, 3, 4, 5, 8, 6))
  a <- gstudy_pxi(d, "p", "i", "y")
  b <- gstudy_crossed(d, "p", "i", "y", facet_labels = "item")
  expect_equal(a$variance_components, b$variance_components, tolerance = 1e-10)
  d$p <- factor(d$p, levels = c(10, 20, 30, 40))
  expect_equal(gstudy_pxi(d, "p", "i", "y")$variance_components, a$variance_components)
})

test_that("error budgets and intervals match hand calculations", {
  gs <- fixture_gt()
  b <- error_budget(gs, c(item = 3))
  expect_equal(b$relative_contribution, c(0, 0, 2))
  expect_equal(b$absolute_contribution, c(0, 1/3, 2))
  se <- sem_gtheory(gs, c(item = 3))
  expect_equal(se$sem, sqrt(c(2, 7/3)))
  ci <- score_interval(gs, 50, c(item = 3))
  expect_equal(ci$lower, 50 - qnorm(.975) * sqrt(7/3))
  expect_error(score_interval(gs, 50, conf = 1), "strictly")
  expect_error(error_budget(gs, c(item = 2.5)), "integers")
  gs$variance_components["item"] <- -1
  expect_error(error_budget(gs), "Negative")
  b <- error_budget(gs, negative = "zero")
  expect_equal(attr(b, "adjusted_components"), "item")
  expect_equal(gs$variance_components[["item"]], -1)
})

test_that("grid, optimization and sensitivity agree with analytic thresholds", {
  gs <- fixture_gt()
  tab <- dstudy_grid(gs, list(item = 1:10))
  expect_equal(tab$g_coefficient, 4 / (4 + 6/(1:10)))
  expect_equal(tab$phi_coefficient, 4 / (4 + 7/(1:10)))
  opt <- optimize_dstudy(gs, list(item = 1:10), target = .8)
  expect_equal(opt$best$item, 7L)
  expect_false(optimize_dstudy(gs, list(item = 1:5), target = .8)$feasible)
  expect_false(optimize_dstudy(gs, list(item = 1:10), budget = 6)$feasible)
  expect_equal(optimize_dstudy(gs, list(item = 1:10), costs = c(item = 2))$best$cost, 21)
  sen <- dstudy_sensitivity(gs, c(item = 2))
  expect_equal(sen$delta_g, 4/6 - 4/7)
  expect_equal(sen$added_observations, 1)
  expect_error(dstudy_grid(gs, list(items = 2)), "every facet")
  gs$variance_components[] <- 0
  expect_true(all(is.na(dstudy_grid(gs, list(item = 1:2))$g_coefficient)))
  expect_false(optimize_dstudy(gs, list(item = 1:2))$feasible)
})

test_that("multifacet formulas and legacy wrappers agree", {
  d <- simulate_gstudy(8, c(item = 3, rater = 2), c(person = 4, residual = 1), seed = 9)
  gs <- gstudy_pxir(d, "person", "item", "rater", "score")
  gs$variance_components <- c(person = 4, item = 1, rater = 2,
    "person:item" = 3, "person:rater" = 4, "item:rater" = 5, residual = 6)
  b <- error_budget(gs, c(item = 3, rater = 2))
  expect_equal(sum(b$relative_contribution), 3/3 + 4/2 + 6/6)
  expect_equal(sum(b$absolute_contribution), 4 + 1/3 + 2/2 + 5/6)
  expect_equal(sum(b$relative_contribution), unname(dstudy_pxir(gs)$relative_error))
  expect_equal(nrow(dstudy_grid(gs, list(item = 2:4, rater = 2:3))), 6)
  g4 <- gstudy_crossed(simulate_gstudy(5, c(item = 2, rater = 2, occasion = 2),
    c(person = 3, residual = 1), seed = 5), "person", c("item", "rater", "occasion"), "score")
  g4$variance_components <- abs(g4$variance_components)
  b4 <- error_budget(g4)
  expect_equal(sum(b4$absolute_contribution), unname(dstudy_crossed(g4)$absolute_error))
})

test_that("nested planning and ID validation work", {
  d <- data.frame(p = rep(c(10, 20, 30), each = 2), i = 1:6, y = c(1, 3, 4, 5, 8, 6))
  gs <- gstudy_nested_ip(d, "p", "i", "y")
  expect_true(check_gstudy_design(d, "p", "i", "y", "nested")$valid)
  b <- error_budget(gs)
  expect_equal(sum(b$absolute_contribution), unname(dstudy_nested_ip(gs)$absolute_error))
  expect_equal(b$relative_contribution, b$absolute_contribution)
  expect_error(bootstrap_gstudy(gs, B = 3), "crossed")
})

test_that("simulation shares effects correctly and preserves RNG state", {
  set.seed(123)
  state <- .Random.seed
  d <- simulate_gstudy(20, c(item = 3), c(person = 4), mean = 10, seed = 7)
  expect_identical(.Random.seed, state)
  expect_equal(d, simulate_gstudy(20, c(item = 3), c(person = 4), mean = 10, seed = 7))
  expect_true(all(vapply(split(d$score, d$person), function(x) length(unique(x)) == 1, logical(1))))
  expect_error(simulate_gstudy(5, c(item = 3), c(residual = -1)), "Invalid")
  expect_error(simulate_gstudy(5, c(item = 3), c(unknown = 1)), "Invalid")
  # Aggregate independent fitted ANOVA estimates to check the generative model.
  fits <- vapply(1:80, function(s) {
    x <- simulate_gstudy(20, c(item = 6), c(person = 4, item = 1, residual = 2), seed = s)
    gstudy_pxi(x, "person", "item", "score")$variance_components
  }, numeric(3))
  expect_equal(unname(rowMeans(fits)), c(4, 1, 2), tolerance = .3)
})

test_that("bootstrap returns reproducible uncertainty and tracks adjustments", {
  gs <- fixture_gt()
  set.seed(55)
  old <- .Random.seed
  b <- bootstrap_gstudy(gs, B = 20, seed = 19)
  expect_identical(.Random.seed, old)
  expect_equal(b, bootstrap_gstudy(gs, B = 20, seed = 19))
  expect_equal(nrow(b$replicates), 20)
  expect_equal(nrow(b$intervals), 9)
  expect_true(all(b$intervals$lower <= b$intervals$upper))
  expect_equal(length(b$replicate_adjustments), 20)
  expect_equal(b$intervals$estimate[1:3], c(4, 1, 6))
  expect_true(any(b$replicate_adjustments > 0))
  proposed <- bootstrap_gstudy(gs, B = 20, seed = 19, design_levels = c(item = 4))
  expect_equal(proposed$replicates[1:3], b$replicates[1:3])
  expect_equal(proposed$replicates$relative_error, b$replicates$relative_error / 2)
  expect_error(bootstrap_gstudy(gs, B = 1), "integers")
})

test_that("multifacet bootstrap, zero boundaries and invalid designs are explicit", {
  d <- simulate_gstudy(6, c(item = 3, rater = 2), c(person = 4, residual = 1), seed = 82)
  gs <- gstudy_pxir(d, "person", "item", "rater", "score")
  b <- bootstrap_gstudy(gs, B = 5, seed = 19, negative = "zero")
  expect_equal(nrow(b$intervals), 13)
  expect_true(all(b$intervals$n_valid == 5))
  gs0 <- fixture_gt()
  gs0$variance_components[] <- 0
  expect_warning(z <- bootstrap_gstudy(gs0, B = 3, seed = 1), "undefined")
  expect_true(all(z$intervals$n_valid[z$intervals$quantity %in% c("g_coefficient", "phi_coefficient")] == 0))
  d$score[1] <- Inf
  expect_error(gstudy_pxir(d, "person", "item", "rater", "score"), "nonfinite")
  expect_error(gstudy_pxi(data.frame(p = 1, i = 1, y = 0), "p", "i", "y"), "at least two")
  expect_error(optimize_dstudy(fixture_gt(), list(item = 2:4), costs = c(item = -1)), "nonnegative")
  expect_error(gstudy_crossed(d, "person", c("item", "rater"), "score", c("a:b", "rater")))
})
