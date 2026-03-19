validate_columns <- function(data, cols) {
  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }
}

as_name <- function(x) {
  if (length(x) != 1L || !is.character(x)) {
    stop("Column arguments must be single character strings.", call. = FALSE)
  }
  x
}

make_gstudy <- function(design, n_persons, n_items, ss, df, ms, variance_components) {
  structure(
    list(
      design = design,
      n_persons = n_persons,
      n_items = n_items,
      ss = ss,
      df = df,
      ms = ms,
      variance_components = variance_components
    ),
    class = "gstudy_gtheoryr"
  )
}

#' Estimate Variance Components for a Crossed Persons-by-Items Design
#'
#' Estimates ANOVA mean squares and variance components for a fully crossed
#' random-effects design with persons crossed with items.
#'
#' @param data A data frame containing one row per person-item observation.
#' @param person Name of the person column.
#' @param item Name of the item column.
#' @param score Name of the numeric score column.
#'
#' @return An object of class `"gstudy_gtheoryr"`.
#' @examples
#' scores <- data.frame(
#'   person = rep(c("P1", "P2", "P3"), each = 3),
#'   item = rep(c("I1", "I2", "I3"), times = 3),
#'   score = c(8, 7, 9, 5, 4, 6, 7, 6, 8)
#' )
#'
#' gs <- gstudy_pxi(scores, person = "person", item = "item", score = "score")
#' gs
#' dstudy_pxi(gs, n_items = 6)
gstudy_pxi <- function(data, person, item, score) {
  person <- as_name(person)
  item <- as_name(item)
  score <- as_name(score)
  validate_columns(data, c(person, item, score))

  dat <- data[, c(person, item, score)]
  names(dat) <- c("person", "item", "score")

  if (!is.numeric(dat$score)) {
    stop("The score column must be numeric.", call. = FALSE)
  }

  counts <- xtabs(~ person + item, data = dat)
  if (any(counts != 1)) {
    stop(
      "Crossed design requires exactly one score per person-item cell in a balanced table.",
      call. = FALSE
    )
  }

  n_persons <- nrow(counts)
  n_items <- ncol(counts)
  grand_mean <- mean(dat$score)

  person_means <- tapply(dat$score, dat$person, mean)
  item_means <- tapply(dat$score, dat$item, mean)

  ss_person <- n_items * sum((person_means - grand_mean)^2)
  ss_item <- n_persons * sum((item_means - grand_mean)^2)

  dat$predicted_main <- grand_mean +
    (person_means[dat$person] - grand_mean) +
    (item_means[dat$item] - grand_mean)
  dat$residual <- dat$score - dat$predicted_main

  ss_residual <- sum(dat$residual^2)
  ss_total <- sum((dat$score - grand_mean)^2)

  df_person <- n_persons - 1
  df_item <- n_items - 1
  df_residual <- df_person * df_item
  df_total <- nrow(dat) - 1

  ms_person <- ss_person / df_person
  ms_item <- ss_item / df_item
  ms_residual <- ss_residual / df_residual

  var_residual <- ms_residual
  var_person <- (ms_person - ms_residual) / n_items
  var_item <- (ms_item - ms_residual) / n_persons

  make_gstudy(
    design = "p x i",
    n_persons = n_persons,
    n_items = n_items,
    ss = c(total = ss_total, person = ss_person, item = ss_item, residual = ss_residual),
    df = c(total = df_total, person = df_person, item = df_item, residual = df_residual),
    ms = c(person = ms_person, item = ms_item, residual = ms_residual),
    variance_components = c(person = var_person, item = var_item, residual = var_residual)
  )
}

#' Estimate Variance Components for a Nested Items-within-Person Design
#'
#' Estimates ANOVA mean squares and variance components for a simple nested
#' design in which each person has their own set of items.
#'
#' @param data A data frame containing one row per observation.
#' @param person Name of the person column.
#' @param item Name of the nested item column.
#' @param score Name of the numeric score column.
#'
#' @return An object of class `"gstudy_gtheoryr"`.
#' @examples
#' nested_scores <- data.frame(
#'   person = c("P1", "P1", "P2", "P2", "P3", "P3"),
#'   item = c("P1_I1", "P1_I2", "P2_I1", "P2_I2", "P3_I1", "P3_I2"),
#'   score = c(8, 6, 5, 4, 9, 7)
#' )
#'
#' gs_nested <- gstudy_nested_ip(
#'   nested_scores,
#'   person = "person",
#'   item = "item",
#'   score = "score"
#' )
#' gs_nested
gstudy_nested_ip <- function(data, person, item, score) {
  person <- as_name(person)
  item <- as_name(item)
  score <- as_name(score)
  validate_columns(data, c(person, item, score))

  dat <- data[, c(person, item, score)]
  names(dat) <- c("person", "item", "score")

  if (!is.numeric(dat$score)) {
    stop("The score column must be numeric.", call. = FALSE)
  }

  items_per_person <- table(dat$person)
  if (length(unique(items_per_person)) != 1L) {
    stop(
      "Nested example requires a balanced number of item observations per person.",
      call. = FALSE
    )
  }

  if (any(duplicated(dat$item))) {
    stop(
      "Nested design expects item labels to be unique within the full dataset.",
      call. = FALSE
    )
  }

  n_persons <- length(items_per_person)
  n_items <- as.integer(items_per_person[[1L]])
  grand_mean <- mean(dat$score)
  person_means <- tapply(dat$score, dat$person, mean)

  ss_person <- n_items * sum((person_means - grand_mean)^2)
  dat$within_person <- dat$score - person_means[dat$person]
  ss_nested <- sum(dat$within_person^2)
  ss_total <- sum((dat$score - grand_mean)^2)

  df_person <- n_persons - 1
  df_nested <- n_persons * (n_items - 1)
  df_total <- nrow(dat) - 1

  ms_person <- ss_person / df_person
  ms_nested <- ss_nested / df_nested

  var_nested <- ms_nested
  var_person <- (ms_person - ms_nested) / n_items

  make_gstudy(
    design = "i:p",
    n_persons = n_persons,
    n_items = n_items,
    ss = c(total = ss_total, person = ss_person, nested_item = ss_nested),
    df = c(total = df_total, person = df_person, nested_item = df_nested),
    ms = c(person = ms_person, nested_item = ms_nested),
    variance_components = c(person = var_person, nested_item = var_nested)
  )
}

#' Design Study for a Crossed Persons-by-Items Design
#'
#' Computes relative and absolute error variances, the generalizability
#' coefficient, and the phi coefficient for a proposed number of items.
#'
#' @param gstudy A result from [gstudy_pxi()].
#' @param n_items Number of items in the proposed design.
#'
#' @return An object of class `"dstudy_gtheoryr"`.
#' @examples
#' scores <- data.frame(
#'   person = rep(c("P1", "P2", "P3"), each = 3),
#'   item = rep(c("I1", "I2", "I3"), times = 3),
#'   score = c(8, 7, 9, 5, 4, 6, 7, 6, 8)
#' )
#'
#' gs <- gstudy_pxi(scores, person = "person", item = "item", score = "score")
#' dstudy_pxi(gs, n_items = 6)
dstudy_pxi <- function(gstudy, n_items = gstudy$n_items) {
  if (!inherits(gstudy, "gstudy_gtheoryr") || !identical(gstudy$design, "p x i")) {
    stop("`gstudy` must come from gstudy_pxi().", call. = FALSE)
  }
  if (!is.numeric(n_items) || length(n_items) != 1L || n_items <= 0) {
    stop("`n_items` must be a single positive number.", call. = FALSE)
  }

  vc <- gstudy$variance_components
  relative_error <- vc["residual"] / n_items
  absolute_error <- (vc["item"] + vc["residual"]) / n_items
  g_coef <- vc["person"] / (vc["person"] + relative_error)
  phi_coef <- vc["person"] / (vc["person"] + absolute_error)

  structure(
    list(
      design = gstudy$design,
      n_items = n_items,
      relative_error = relative_error,
      absolute_error = absolute_error,
      g_coefficient = g_coef,
      phi_coefficient = phi_coef
    ),
    class = "dstudy_gtheoryr"
  )
}

#' Design Study for a Nested Items-within-Person Design
#'
#' Computes a simple reliability summary for a proposed number of nested items
#' per person.
#'
#' @param gstudy A result from [gstudy_nested_ip()].
#' @param n_items Number of nested items per person in the proposed design.
#'
#' @return An object of class `"dstudy_gtheoryr"`.
#' @examples
#' nested_scores <- data.frame(
#'   person = c("P1", "P1", "P2", "P2", "P3", "P3"),
#'   item = c("P1_I1", "P1_I2", "P2_I1", "P2_I2", "P3_I1", "P3_I2"),
#'   score = c(8, 6, 5, 4, 9, 7)
#' )
#'
#' gs_nested <- gstudy_nested_ip(
#'   nested_scores,
#'   person = "person",
#'   item = "item",
#'   score = "score"
#' )
#' dstudy_nested_ip(gs_nested, n_items = 4)
dstudy_nested_ip <- function(gstudy, n_items = gstudy$n_items) {
  if (!inherits(gstudy, "gstudy_gtheoryr") || !identical(gstudy$design, "i:p")) {
    stop("`gstudy` must come from gstudy_nested_ip().", call. = FALSE)
  }
  if (!is.numeric(n_items) || length(n_items) != 1L || n_items <= 0) {
    stop("`n_items` must be a single positive number.", call. = FALSE)
  }

  vc <- gstudy$variance_components
  relative_error <- vc["nested_item"] / n_items
  g_coef <- vc["person"] / (vc["person"] + relative_error)

  structure(
    list(
      design = gstudy$design,
      n_items = n_items,
      relative_error = relative_error,
      absolute_error = relative_error,
      g_coefficient = g_coef,
      phi_coefficient = g_coef
    ),
    class = "dstudy_gtheoryr"
  )
}

#' @export
print.gstudy_gtheoryr <- function(x, ...) {
  cat("Generalizability Study\n")
  cat("Design:", x$design, "\n")
  cat("Persons:", x$n_persons, " Items per person:", x$n_items, "\n\n")
  cat("ANOVA components\n")
  print(data.frame(
    source = names(x$ss),
    ss = unname(x$ss),
    df = unname(x$df[names(x$ss)]),
    row.names = NULL
  ))
  cat("\nMean squares\n")
  print(data.frame(
    source = names(x$ms),
    ms = unname(x$ms),
    row.names = NULL
  ))
  cat("\nVariance components\n")
  print(data.frame(
    component = names(x$variance_components),
    estimate = unname(x$variance_components),
    row.names = NULL
  ))
  invisible(x)
}

#' @export
print.dstudy_gtheoryr <- function(x, ...) {
  cat("Design Study\n")
  cat("Design:", x$design, "\n")
  cat("Proposed items:", x$n_items, "\n\n")
  print(data.frame(
    quantity = c("relative_error", "absolute_error", "g_coefficient", "phi_coefficient"),
    estimate = c(x$relative_error, x$absolute_error, x$g_coefficient, x$phi_coefficient),
    row.names = NULL
  ))
  invisible(x)
}
