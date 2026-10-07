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

as_label <- function(x, default = "facet") {
  if (missing(x) || is.null(x) || identical(x, "")) {
    return(default)
  }
  as_name(x)
}

validate_positive_scalar <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x <= 0) {
    stop("`", name, "` must be a single positive number.", call. = FALSE)
  }
}

safe_plural <- function(x) {
  if (identical(x, "person")) {
    return("persons")
  }
  if (identical(x, "occasion")) {
    return("occasions")
  }
  paste0(x, "s")
}

make_gstudy <- function(
  design,
  ss,
  df,
  ms,
  variance_components,
  facet_counts,
  count_fields,
  facet_name = NULL
) {
  out <- list(
    design = design,
    facet_counts = facet_counts,
    ss = ss,
    df = df,
    ms = ms,
    variance_components = variance_components,
    facet_name = facet_name
  )
  out[names(count_fields)] <- count_fields
  structure(out, class = "gstudy_gtheoryr")
}

make_dstudy <- function(
  design,
  design_counts,
  relative_error,
  absolute_error,
  g_coefficient,
  phi_coefficient,
  count_fields,
  facet_name = NULL
) {
  out <- list(
    design = design,
    design_counts = design_counts,
    relative_error = relative_error,
    absolute_error = absolute_error,
    g_coefficient = g_coefficient,
    phi_coefficient = phi_coefficient,
    facet_name = facet_name
  )
  out[names(count_fields)] <- count_fields
  structure(out, class = "dstudy_gtheoryr")
}

gstudy_anova_table <- function(x) {
  data.frame(
    source = names(x$ss),
    ss = unname(x$ss),
    df = unname(x$df[names(x$ss)]),
    row.names = NULL
  )
}

gstudy_mean_squares_table <- function(x) {
  data.frame(
    source = names(x$ms),
    ms = unname(x$ms),
    row.names = NULL
  )
}

gstudy_variance_components_table <- function(x) {
  data.frame(
    component = names(x$variance_components),
    estimate = unname(x$variance_components),
    row.names = NULL
  )
}

format_design_counts <- function(x) {
  paste(
    paste(names(x), unname(x), sep = ": "),
    collapse = "  "
  )
}

threefacet_components <- function(dat) {
  fit <- stats::aov(score ~ person * item * facet, data = dat)
  tab <- summary(fit)[[1L]]
  rownames(tab) <- trimws(rownames(tab))

  ss <- c(
    total = sum((dat$score - mean(dat$score))^2),
    person = tab["person", "Sum Sq"],
    item = tab["item", "Sum Sq"],
    facet = tab["facet", "Sum Sq"],
    "person:item" = tab["person:item", "Sum Sq"],
    "person:facet" = tab["person:facet", "Sum Sq"],
    "item:facet" = tab["item:facet", "Sum Sq"],
    residual = tab["person:item:facet", "Sum Sq"]
  )

  df <- c(
    total = nrow(dat) - 1,
    person = tab["person", "Df"],
    item = tab["item", "Df"],
    facet = tab["facet", "Df"],
    "person:item" = tab["person:item", "Df"],
    "person:facet" = tab["person:facet", "Df"],
    "item:facet" = tab["item:facet", "Df"],
    residual = tab["person:item:facet", "Df"]
  )

  ms <- c(
    person = tab["person", "Mean Sq"],
    item = tab["item", "Mean Sq"],
    facet = tab["facet", "Mean Sq"],
    "person:item" = tab["person:item", "Mean Sq"],
    "person:facet" = tab["person:facet", "Mean Sq"],
    "item:facet" = tab["item:facet", "Mean Sq"],
    residual = tab["person:item:facet", "Mean Sq"]
  )

  list(ss = ss, df = df, ms = ms)
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
#' dstudy_pxi(gs)
gstudy_pxi <- function(data, person, item, score) {
  gt_require_design(data, person, item, score)
  person <- as_name(person)
  item <- as_name(item)
  score <- as_name(score)
  validate_columns(data, c(person, item, score))

  dat <- data[, c(person, item, score)]
  names(dat) <- c("person", "item", "score")
  dat$person <- as.character(dat$person)
  dat$item <- as.character(dat$item)

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
    (person_means[as.character(dat$person)] - grand_mean) +
    (item_means[as.character(dat$item)] - grand_mean)
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
    ss = c(total = ss_total, person = ss_person, item = ss_item, residual = ss_residual),
    df = c(total = df_total, person = df_person, item = df_item, residual = df_residual),
    ms = c(person = ms_person, item = ms_item, residual = ms_residual),
    variance_components = c(person = var_person, item = var_item, residual = var_residual),
    facet_counts = c(persons = n_persons, items = n_items),
    count_fields = list(n_persons = n_persons, n_items = n_items)
  )
}

#' Estimate Variance Components for a Crossed Persons-by-Items-by-Facet Design
#'
#' Estimates ANOVA mean squares and variance components for a fully crossed
#' random-effects design with persons crossed with items and one additional
#' facet such as raters, occasions, or forms.
#'
#' @param data A data frame containing one row per person-item-facet observation.
#' @param person Name of the person column.
#' @param item Name of the item column.
#' @param facet Name of the third facet column, such as a rater or occasion.
#' @param score Name of the numeric score column.
#' @param facet_name A user-facing label for the third facet. Defaults to the
#'   value supplied to `facet`.
#'
#' @return An object of class `"gstudy_gtheoryr"`.
#' @examples
#' scores <- expand.grid(
#'   person = c("P1", "P2", "P3"),
#'   item = c("I1", "I2"),
#'   rater = c("R1", "R2"),
#'   stringsAsFactors = FALSE
#' )
#' scores$score <- c(8, 7, 7, 6, 5, 4, 6, 5, 7, 6, 8, 7)
#'
#' gs <- gstudy_pxif(
#'   scores,
#'   person = "person",
#'   item = "item",
#'   facet = "rater",
#'   score = "score",
#'   facet_name = "rater"
#' )
#' gs
#' dstudy_pxif(gs)
gstudy_pxif <- function(data, person, item, facet, score, facet_name = facet) {
  gt_require_design(data, person, c(item, facet), score)
  validate_facet_inputs(c(item, facet), c("item", facet_name))
  person <- as_name(person)
  item <- as_name(item)
  facet <- as_name(facet)
  score <- as_name(score)
  facet_name <- as_label(facet_name, default = facet)
  validate_columns(data, c(person, item, facet, score))

  dat <- data[, c(person, item, facet, score)]
  names(dat) <- c("person", "item", "facet", "score")

  if (!is.numeric(dat$score)) {
    stop("The score column must be numeric.", call. = FALSE)
  }

  dat$person <- factor(dat$person)
  dat$item <- factor(dat$item)
  dat$facet <- factor(dat$facet)

  counts <- xtabs(~ person + item + facet, data = dat)
  if (any(counts != 1)) {
    stop(
      "Crossed three-facet design requires exactly one score per person-item-facet cell in a balanced table.",
      call. = FALSE
    )
  }

  n_persons <- dim(counts)[1]
  n_items <- dim(counts)[2]
  n_facets <- dim(counts)[3]
  aov_parts <- threefacet_components(dat)

  ms <- aov_parts$ms
  var_residual <- ms["residual"]
  var_person_item <- (ms["person:item"] - ms["residual"]) / n_facets
  var_person_facet <- (ms["person:facet"] - ms["residual"]) / n_items
  var_item_facet <- (ms["item:facet"] - ms["residual"]) / n_persons
  var_person <- (ms["person"] - ms["person:item"] - ms["person:facet"] + ms["residual"]) / (n_items * n_facets)
  var_item <- (ms["item"] - ms["person:item"] - ms["item:facet"] + ms["residual"]) / (n_persons * n_facets)
  var_facet <- (ms["facet"] - ms["person:facet"] - ms["item:facet"] + ms["residual"]) / (n_persons * n_items)

  vc <- c(
    unname(var_person),
    unname(var_item),
    unname(var_facet),
    unname(var_person_item),
    unname(var_person_facet),
    unname(var_item_facet),
    unname(var_residual)
  )
  names(vc) <- c(
    "person",
    "item",
    facet_name,
    "person:item",
    paste0("person:", facet_name),
    paste0("item:", facet_name),
    "residual"
  )

  ss <- aov_parts$ss
  df <- aov_parts$df
  ms_out <- ms
  names(ss)[names(ss) == "facet"] <- facet_name
  names(ss)[names(ss) == "person:facet"] <- paste0("person:", facet_name)
  names(ss)[names(ss) == "item:facet"] <- paste0("item:", facet_name)
  names(df)[names(df) == "facet"] <- facet_name
  names(df)[names(df) == "person:facet"] <- paste0("person:", facet_name)
  names(df)[names(df) == "item:facet"] <- paste0("item:", facet_name)
  names(ms_out)[names(ms_out) == "facet"] <- facet_name
  names(ms_out)[names(ms_out) == "person:facet"] <- paste0("person:", facet_name)
  names(ms_out)[names(ms_out) == "item:facet"] <- paste0("item:", facet_name)

  make_gstudy(
    design = "p x i x f",
    ss = ss,
    df = df,
    ms = ms_out,
    variance_components = vc,
    facet_counts = c(
      persons = n_persons,
      items = n_items,
      stats::setNames(n_facets, safe_plural(facet_name))
    ),
    count_fields = list(n_persons = n_persons, n_items = n_items, n_facets = n_facets),
    facet_name = facet_name
  )
}

#' Estimate Variance Components for a Crossed Persons-by-Items-by-Raters Design
#'
#' Convenience wrapper around [gstudy_pxif()] for a crossed persons-by-items-by-raters design.
#'
#' @param data A data frame containing one row per person-item-rater observation.
#' @param person Name of the person column.
#' @param item Name of the item column.
#' @param rater Name of the rater column.
#' @param score Name of the numeric score column.
#'
#' @return An object of class `"gstudy_gtheoryr"`.
#' @examples
#' scores <- expand.grid(
#'   person = c("P1", "P2", "P3"),
#'   item = c("I1", "I2"),
#'   rater = c("R1", "R2"),
#'   stringsAsFactors = FALSE
#' )
#' scores$score <- c(8, 7, 7, 6, 5, 4, 6, 5, 7, 6, 8, 7)
#'
#' gs <- gstudy_pxir(scores, person = "person", item = "item", rater = "rater", score = "score")
#' gs
gstudy_pxir <- function(data, person, item, rater, score) {
  gstudy_pxif(
    data = data,
    person = person,
    item = item,
    facet = rater,
    score = score,
    facet_name = "rater"
  )
}

#' Estimate Variance Components for a Crossed Persons-by-Items-by-Occasions Design
#'
#' Convenience wrapper around [gstudy_pxif()] for a crossed persons-by-items-by-occasions design.
#'
#' @param data A data frame containing one row per person-item-occasion observation.
#' @param person Name of the person column.
#' @param item Name of the item column.
#' @param occasion Name of the occasion column.
#' @param score Name of the numeric score column.
#'
#' @return An object of class `"gstudy_gtheoryr"`.
#' @examples
#' scores <- expand.grid(
#'   person = c("P1", "P2", "P3"),
#'   item = c("I1", "I2"),
#'   occasion = c("T1", "T2"),
#'   stringsAsFactors = FALSE
#' )
#' scores$score <- c(8, 8, 7, 7, 5, 5, 6, 6, 7, 7, 8, 8)
#'
#' gs <- gstudy_pxio(
#'   scores,
#'   person = "person",
#'   item = "item",
#'   occasion = "occasion",
#'   score = "score"
#' )
#' gs
gstudy_pxio <- function(data, person, item, occasion, score) {
  gstudy_pxif(
    data = data,
    person = person,
    item = item,
    facet = occasion,
    score = score,
    facet_name = "occasion"
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
  gt_require_design(data, person, item, score, "nested")
  person <- as_name(person)
  item <- as_name(item)
  score <- as_name(score)
  validate_columns(data, c(person, item, score))

  dat <- data[, c(person, item, score)]
  names(dat) <- c("person", "item", "score")
  dat$person <- as.character(dat$person)
  dat$item <- as.character(dat$item)

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
  dat$within_person <- dat$score - person_means[as.character(dat$person)]
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
    ss = c(total = ss_total, person = ss_person, nested_item = ss_nested),
    df = c(total = df_total, person = df_person, nested_item = df_nested),
    ms = c(person = ms_person, nested_item = ms_nested),
    variance_components = c(person = var_person, nested_item = var_nested),
    facet_counts = c(persons = n_persons, nested_items = n_items),
    count_fields = list(n_persons = n_persons, n_items = n_items)
  )
}

#' Design Study for a Crossed Persons-by-Items Design
#'
#' Computes relative and absolute error variances, the generalizability
#' coefficient, and the phi coefficient for the current or proposed number of
#' items.
#'
#' @param gstudy A result from [gstudy_pxi()].
#' @param n_items Number of items in the design. Defaults to the current number
#'   of items in the supplied `gstudy` object.
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
#' dstudy_pxi(gs)
dstudy_pxi <- function(gstudy, n_items = gstudy$n_items) {
  if (!inherits(gstudy, "gstudy_gtheoryr") || !identical(gstudy$design, "p x i")) {
    stop("`gstudy` must come from gstudy_pxi().", call. = FALSE)
  }
  validate_positive_scalar(n_items, "n_items")

  vc <- gstudy$variance_components
  relative_error <- vc["residual"] / n_items
  absolute_error <- (vc["item"] + vc["residual"]) / n_items
  g_coef <- vc["person"] / (vc["person"] + relative_error)
  phi_coef <- vc["person"] / (vc["person"] + absolute_error)

  make_dstudy(
    design = gstudy$design,
    design_counts = c(items = n_items),
    relative_error = relative_error,
    absolute_error = absolute_error,
    g_coefficient = g_coef,
    phi_coefficient = phi_coef,
    count_fields = list(n_items = n_items)
  )
}

#' Design Study for a Crossed Persons-by-Items-by-Facet Design
#'
#' Computes relative and absolute error variances, the generalizability
#' coefficient, and the phi coefficient for the current or proposed number of
#' items and levels of a third crossed facet such as raters, occasions, or forms.
#'
#' @param gstudy A result from [gstudy_pxif()].
#' @param n_items Number of items in the design. Defaults to the current number
#'   of items in the supplied `gstudy` object.
#' @param n_facets Number of levels of the third facet. Defaults to the current
#'   number in the supplied `gstudy` object.
#'
#' @return An object of class `"dstudy_gtheoryr"`.
#' @examples
#' scores <- expand.grid(
#'   person = c("P1", "P2", "P3"),
#'   item = c("I1", "I2"),
#'   rater = c("R1", "R2"),
#'   stringsAsFactors = FALSE
#' )
#' scores$score <- c(8, 7, 7, 6, 5, 4, 6, 5, 7, 6, 8, 7)
#'
#' gs <- gstudy_pxif(
#'   scores,
#'   person = "person",
#'   item = "item",
#'   facet = "rater",
#'   score = "score",
#'   facet_name = "rater"
#' )
#' dstudy_pxif(gs)
dstudy_pxif <- function(gstudy, n_items = gstudy$n_items, n_facets = gstudy$n_facets) {
  if (!inherits(gstudy, "gstudy_gtheoryr") || !identical(gstudy$design, "p x i x f")) {
    stop("`gstudy` must come from gstudy_pxif().", call. = FALSE)
  }
  validate_positive_scalar(n_items, "n_items")
  validate_positive_scalar(n_facets, "n_facets")

  facet_name <- gstudy$facet_name
  vc <- gstudy$variance_components

  relative_error <-
    vc["person:item"] / n_items +
    vc[paste0("person:", facet_name)] / n_facets +
    vc["residual"] / (n_items * n_facets)

  absolute_error <-
    vc["item"] / n_items +
    vc[facet_name] / n_facets +
    vc[paste0("item:", facet_name)] / (n_items * n_facets) +
    relative_error

  g_coef <- vc["person"] / (vc["person"] + relative_error)
  phi_coef <- vc["person"] / (vc["person"] + absolute_error)

  make_dstudy(
    design = gstudy$design,
    design_counts = c(
      items = n_items,
      stats::setNames(n_facets, safe_plural(facet_name))
    ),
    relative_error = relative_error,
    absolute_error = absolute_error,
    g_coefficient = g_coef,
    phi_coefficient = phi_coef,
    count_fields = list(n_items = n_items, n_facets = n_facets),
    facet_name = facet_name
  )
}

#' Design Study for a Crossed Persons-by-Items-by-Raters Design
#'
#' Convenience wrapper around [dstudy_pxif()] for a current or proposed
#' persons-by-items-by-raters design.
#'
#' @param gstudy A result from [gstudy_pxir()].
#' @param n_items Number of items in the design. Defaults to the current number
#'   in the supplied `gstudy` object.
#' @param n_raters Number of raters in the design. Defaults to the current
#'   number in the supplied `gstudy` object.
#'
#' @return An object of class `"dstudy_gtheoryr"`.
#' @examples
#' scores <- expand.grid(
#'   person = c("P1", "P2", "P3"),
#'   item = c("I1", "I2"),
#'   rater = c("R1", "R2"),
#'   stringsAsFactors = FALSE
#' )
#' scores$score <- c(8, 7, 7, 6, 5, 4, 6, 5, 7, 6, 8, 7)
#'
#' gs <- gstudy_pxir(scores, person = "person", item = "item", rater = "rater", score = "score")
#' dstudy_pxir(gs)
dstudy_pxir <- function(gstudy, n_items = gstudy$n_items, n_raters = gstudy$n_facets) {
  if (!inherits(gstudy, "gstudy_gtheoryr") || !identical(gstudy$facet_name, "rater")) {
    stop("`gstudy` must come from gstudy_pxir().", call. = FALSE)
  }
  dstudy_pxif(gstudy, n_items = n_items, n_facets = n_raters)
}

#' Design Study for a Crossed Persons-by-Items-by-Occasions Design
#'
#' Convenience wrapper around [dstudy_pxif()] for a current or proposed
#' persons-by-items-by-occasions design.
#'
#' @param gstudy A result from [gstudy_pxio()].
#' @param n_items Number of items in the design. Defaults to the current number
#'   in the supplied `gstudy` object.
#' @param n_occasions Number of occasions in the design. Defaults to the current
#'   number in the supplied `gstudy` object.
#'
#' @return An object of class `"dstudy_gtheoryr"`.
#' @examples
#' scores <- expand.grid(
#'   person = c("P1", "P2", "P3"),
#'   item = c("I1", "I2"),
#'   occasion = c("T1", "T2"),
#'   stringsAsFactors = FALSE
#' )
#' scores$score <- c(8, 8, 7, 7, 5, 5, 6, 6, 7, 7, 8, 8)
#'
#' gs <- gstudy_pxio(
#'   scores,
#'   person = "person",
#'   item = "item",
#'   occasion = "occasion",
#'   score = "score"
#' )
#' dstudy_pxio(gs)
dstudy_pxio <- function(gstudy, n_items = gstudy$n_items, n_occasions = gstudy$n_facets) {
  if (!inherits(gstudy, "gstudy_gtheoryr") || !identical(gstudy$facet_name, "occasion")) {
    stop("`gstudy` must come from gstudy_pxio().", call. = FALSE)
  }
  dstudy_pxif(gstudy, n_items = n_items, n_facets = n_occasions)
}

#' Design Study for a Nested Items-within-Person Design
#'
#' Computes a simple reliability summary for the current or proposed number of
#' nested items per person.
#'
#' @param gstudy A result from [gstudy_nested_ip()].
#' @param n_items Number of nested items per person in the design. Defaults to
#'   the current number in the supplied `gstudy` object.
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
#' dstudy_nested_ip(gs_nested)
dstudy_nested_ip <- function(gstudy, n_items = gstudy$n_items) {
  if (!inherits(gstudy, "gstudy_gtheoryr") || !identical(gstudy$design, "i:p")) {
    stop("`gstudy` must come from gstudy_nested_ip().", call. = FALSE)
  }
  validate_positive_scalar(n_items, "n_items")

  vc <- gstudy$variance_components
  relative_error <- vc["nested_item"] / n_items
  g_coef <- vc["person"] / (vc["person"] + relative_error)

  make_dstudy(
    design = gstudy$design,
    design_counts = c(nested_items = n_items),
    relative_error = relative_error,
    absolute_error = relative_error,
    g_coefficient = g_coef,
    phi_coefficient = g_coef,
    count_fields = list(n_items = n_items)
  )
}

all_factor_subsets <- function(factors) {
  out <- list()
  for (m in seq_along(factors)) {
    out <- c(out, utils::combn(factors, m, simplify = FALSE))
  }
  out
}

effect_name <- function(parts) {
  paste(parts, collapse = ":")
}

design_abbrev <- function(facet_labels) {
  paste(c("p", substring(facet_labels, 1, 1)), collapse = " x ")
}

display_effect_name <- function(parts, label_map, full_effect_name) {
  if (identical(effect_name(parts), full_effect_name)) {
    return("residual")
  }
  paste(unname(label_map[parts]), collapse = ":")
}

validate_facet_inputs <- function(facets, facet_labels) {
  if (!is.character(facets) || length(facets) < 1L) {
    stop("`facets` must be a character vector of one or more facet column names.", call. = FALSE)
  }
  if (length(unique(facets)) != length(facets)) {
    stop("`facets` must not contain duplicate column names.", call. = FALSE)
  }
  if (missing(facet_labels) || is.null(facet_labels)) {
    facet_labels <- facets
  }
  if (!is.character(facet_labels) || length(facet_labels) != length(facets)) {
    stop("`facet_labels` must be a character vector with the same length as `facets`.", call. = FALSE)
  }
  if (anyNA(facet_labels) || any(!nzchar(facet_labels)) ||
      anyDuplicated(facet_labels) > 0L || any(facet_labels %in% c("person", "residual", "score")) ||
      any(grepl(":", facet_labels, fixed = TRUE))) {
    stop("`facet_labels` must be unique, nonempty, and cannot include reserved names or colons.", call. = FALSE)
  }
  facet_labels
}

build_crossed_gstudy <- function(data, person, facets, score, facet_labels = facets) {
  gt_require_design(data, person, facets, score)
  person <- as_name(person)
  score <- as_name(score)
  facet_labels <- validate_facet_inputs(facets, facet_labels)
  validate_columns(data, c(person, facets, score))

  internal_facets <- paste0("facet", seq_along(facets))
  dat <- data[, c(person, facets, score)]
  names(dat) <- c("person", internal_facets, "score")

  if (!is.numeric(dat$score)) {
    stop("The score column must be numeric.", call. = FALSE)
  }

  dat$person <- factor(dat$person)
  for (nm in internal_facets) {
    dat[[nm]] <- factor(dat[[nm]])
  }

  xtabs_formula <- stats::as.formula(
    paste("~", paste(c("person", internal_facets), collapse = " + "))
  )
  counts <- stats::xtabs(xtabs_formula, data = dat)
  if (any(counts != 1)) {
    stop(
      "Crossed design requires exactly one score per cell in a balanced table.",
      call. = FALSE
    )
  }

  level_counts <- dim(counts)
  names(level_counts) <- c("person", internal_facets)
  n_persons <- as.integer(level_counts["person"])
  facet_count_values <- as.integer(level_counts[internal_facets])
  names(facet_count_values) <- facet_labels

  fit_formula <- stats::as.formula(
    paste("score ~", paste(c("person", internal_facets), collapse = " * "))
  )
  fit <- stats::aov(fit_formula, data = dat)
  tab <- summary(fit)[[1L]]
  rownames(tab) <- trimws(rownames(tab))

  all_factors <- c("person", internal_facets)
  subsets <- all_factor_subsets(all_factors)
  subset_names <- vapply(subsets, effect_name, character(1))
  full_effect_name <- effect_name(all_factors)

  ms_internal <- stats::setNames(tab[subset_names, "Mean Sq"], subset_names)
  ss_internal <- stats::setNames(tab[subset_names, "Sum Sq"], subset_names)
  df_internal <- stats::setNames(tab[subset_names, "Df"], subset_names)

  vc_internal <- stats::setNames(numeric(length(subset_names)), subset_names)
  order_idx <- order(vapply(subsets, length, integer(1)), decreasing = TRUE)

  for (idx in order_idx) {
    effect <- subsets[[idx]]
    effect_id <- subset_names[[idx]]
    supersets <- which(vapply(
      subsets,
      function(x) length(x) > length(effect) && all(effect %in% x),
      logical(1)
    ))

    subtract_term <- 0
    if (length(supersets) > 0) {
      for (j in supersets) {
        sup <- subsets[[j]]
        sup_id <- subset_names[[j]]
        coeff <- prod(level_counts[setdiff(all_factors, sup)])
        subtract_term <- subtract_term + coeff * vc_internal[[sup_id]]
      }
    }

    coeff_self <- prod(level_counts[setdiff(all_factors, effect)])
    vc_internal[[effect_id]] <- (ms_internal[[effect_id]] - subtract_term) / coeff_self
  }

  label_map <- c(person = "person", stats::setNames(facet_labels, internal_facets))
  display_names <- vapply(
    subsets,
    function(x) display_effect_name(x, label_map, full_effect_name),
    character(1)
  )

  ss_display <- c(total = sum((dat$score - mean(dat$score))^2), unname(ss_internal))
  names(ss_display)[-1] <- display_names
  df_display <- c(total = nrow(dat) - 1, unname(df_internal))
  names(df_display)[-1] <- display_names
  ms_display <- unname(ms_internal)
  names(ms_display) <- display_names
  vc_display <- unname(vc_internal)
  names(vc_display) <- display_names

  component_factors <- lapply(subsets, function(x) unname(label_map[x]))
  names(component_factors) <- display_names

  gstudy <- make_gstudy(
    design = design_abbrev(facet_labels),
    ss = ss_display,
    df = df_display,
    ms = ms_display,
    variance_components = vc_display,
    facet_counts = c(
      persons = n_persons,
      stats::setNames(facet_count_values, safe_plural(facet_labels))
    ),
    count_fields = c(
      list(n_persons = n_persons),
      as.list(stats::setNames(facet_count_values, paste0("n_", safe_plural(facet_labels))))
    )
  )

  gstudy$facet_labels <- facet_labels
  gstudy$design_levels <- facet_count_values
  gstudy$component_factors <- component_factors
  gstudy$object_of_measurement <- "person"
  gstudy
}

#' Estimate Variance Components for a Generic Crossed Design
#'
#' Estimates ANOVA mean squares and variance components for a balanced crossed
#' random-effects design with persons as the object of measurement and one or
#' more additional facets.
#'
#' @param data A data frame containing one row per observed cell.
#' @param person Name of the person column.
#' @param facets Character vector of facet column names, such as items, raters,
#'   occasions, or forms.
#' @param score Name of the numeric score column.
#' @param facet_labels Optional user-facing labels for the supplied facets.
#'
#' @return An object of class `"gstudy_gtheoryr"`.
#' @examples
#' scores <- expand.grid(
#'   person = c("P1", "P2", "P3"),
#'   item = c("I1", "I2"),
#'   rater = c("R1", "R2"),
#'   occasion = c("T1", "T2"),
#'   stringsAsFactors = FALSE
#' )
#' scores$score <- c(
#'   8, 8, 7, 7, 7, 7, 6, 6,
#'   5, 5, 4, 4, 6, 6, 5, 5,
#'   7, 7, 6, 6, 8, 8, 7, 7
#' )
#'
#' gs <- gstudy_crossed(
#'   scores,
#'   person = "person",
#'   facets = c("item", "rater", "occasion"),
#'   score = "score",
#'   facet_labels = c("item", "rater", "occasion")
#' )
#' gs
gstudy_crossed <- function(data, person, facets, score, facet_labels = facets) {
  build_crossed_gstudy(
    data = data,
    person = person,
    facets = facets,
    score = score,
    facet_labels = facet_labels
  )
}

#' Design Study for a Generic Crossed Design
#'
#' Computes relative and absolute error variances, the generalizability
#' coefficient, and the phi coefficient for the current or proposed levels of
#' the additional facets in a crossed design.
#'
#' @param gstudy A result from [gstudy_crossed()] or one of its wrappers.
#' @param design_levels Optional named numeric vector giving the facet counts to
#'   use in the D-study. Defaults to the current design in `gstudy`.
#'
#' @return An object of class `"dstudy_gtheoryr"`.
#' @examples
#' scores <- expand.grid(
#'   person = c("P1", "P2", "P3"),
#'   item = c("I1", "I2"),
#'   rater = c("R1", "R2"),
#'   occasion = c("T1", "T2"),
#'   stringsAsFactors = FALSE
#' )
#' scores$score <- c(
#'   8, 8, 7, 7, 7, 7, 6, 6,
#'   5, 5, 4, 4, 6, 6, 5, 5,
#'   7, 7, 6, 6, 8, 8, 7, 7
#' )
#'
#' gs <- gstudy_crossed(
#'   scores,
#'   person = "person",
#'   facets = c("item", "rater", "occasion"),
#'   score = "score",
#'   facet_labels = c("item", "rater", "occasion")
#' )
#' dstudy_crossed(gs)
dstudy_crossed <- function(gstudy, design_levels = gstudy$design_levels) {
  if (!inherits(gstudy, "gstudy_gtheoryr") || is.null(gstudy$component_factors)) {
    stop("`gstudy` must come from gstudy_crossed() or one of its crossed-design wrappers.", call. = FALSE)
  }

  if (is.list(design_levels)) {
    design_levels <- unlist(design_levels, use.names = TRUE)
  }
  if (!is.numeric(design_levels) || is.null(names(design_levels))) {
    stop("`design_levels` must be a named numeric vector.", call. = FALSE)
  }

  needed <- gstudy$facet_labels
  if (!setequal(names(design_levels), needed)) {
    stop(
      "`design_levels` must be named with these facets: ",
      paste(needed, collapse = ", "),
      call. = FALSE
    )
  }
  design_levels <- design_levels[needed]
  for (nm in names(design_levels)) {
    validate_positive_scalar(design_levels[[nm]], nm)
  }

  vc <- gstudy$variance_components
  component_factors <- gstudy$component_factors

  person_idx <- which(vapply(component_factors, function(x) identical(x, "person"), logical(1)))
  person_var <- vc[[person_idx]]

  relative_error <- 0
  absolute_error <- 0

  for (nm in names(component_factors)) {
    factors_here <- component_factors[[nm]]
    if (identical(factors_here, "person")) {
      next
    }

    other_facets <- setdiff(factors_here, "person")
    denom <- if (length(other_facets) == 0) 1 else prod(design_levels[other_facets])
    contribution <- vc[[nm]] / denom

    if ("person" %in% factors_here) {
      relative_error <- relative_error + contribution
    }
    absolute_error <- absolute_error + contribution
  }

  g_coef <- person_var / (person_var + relative_error)
  phi_coef <- person_var / (person_var + absolute_error)

  make_dstudy(
    design = gstudy$design,
    design_counts = stats::setNames(unname(design_levels), safe_plural(names(design_levels))),
    relative_error = relative_error,
    absolute_error = absolute_error,
    g_coefficient = g_coef,
    phi_coefficient = phi_coef,
    count_fields = as.list(stats::setNames(unname(design_levels), paste0("n_", safe_plural(names(design_levels)))))
  )
}

#' Estimate Variance Components for a Crossed Persons-by-Items-by-Raters-by-Occasions Design
#'
#' Convenience wrapper around [gstudy_crossed()] for a design with persons,
#' items, raters, and occasions.
#'
#' @param data A data frame containing one row per person-item-rater-occasion observation.
#' @param person Name of the person column.
#' @param item Name of the item column.
#' @param rater Name of the rater column.
#' @param occasion Name of the occasion column.
#' @param score Name of the numeric score column.
#'
#' @return An object of class `"gstudy_gtheoryr"`.
#' @examples
#' scores <- expand.grid(
#'   person = c("P1", "P2", "P3"),
#'   item = c("I1", "I2"),
#'   rater = c("R1", "R2"),
#'   occasion = c("T1", "T2"),
#'   stringsAsFactors = FALSE
#' )
#' scores$score <- c(
#'   8, 8, 7, 7, 7, 7, 6, 6,
#'   5, 5, 4, 4, 6, 6, 5, 5,
#'   7, 7, 6, 6, 8, 8, 7, 7
#' )
#'
#' gs <- gstudy_pxiro(
#'   scores,
#'   person = "person",
#'   item = "item",
#'   rater = "rater",
#'   occasion = "occasion",
#'   score = "score"
#' )
#' gs
gstudy_pxiro <- function(data, person, item, rater, occasion, score) {
  gstudy_crossed(
    data = data,
    person = person,
    facets = c(item, rater, occasion),
    score = score,
    facet_labels = c("item", "rater", "occasion")
  )
}

#' Design Study for a Crossed Persons-by-Items-by-Raters-by-Occasions Design
#'
#' Convenience wrapper around [dstudy_crossed()] for the current or proposed
#' persons-by-items-by-raters-by-occasions design.
#'
#' @param gstudy A result from [gstudy_pxiro()].
#' @param n_items Number of items in the design. Defaults to the current number.
#' @param n_raters Number of raters in the design. Defaults to the current number.
#' @param n_occasions Number of occasions in the design. Defaults to the current number.
#'
#' @return An object of class `"dstudy_gtheoryr"`.
#' @examples
#' scores <- expand.grid(
#'   person = c("P1", "P2", "P3"),
#'   item = c("I1", "I2"),
#'   rater = c("R1", "R2"),
#'   occasion = c("T1", "T2"),
#'   stringsAsFactors = FALSE
#' )
#' scores$score <- c(
#'   8, 8, 7, 7, 7, 7, 6, 6,
#'   5, 5, 4, 4, 6, 6, 5, 5,
#'   7, 7, 6, 6, 8, 8, 7, 7
#' )
#'
#' gs <- gstudy_pxiro(
#'   scores,
#'   person = "person",
#'   item = "item",
#'   rater = "rater",
#'   occasion = "occasion",
#'   score = "score"
#' )
#' dstudy_pxiro(gs)
dstudy_pxiro <- function(
  gstudy,
  n_items = gstudy$design_levels[["item"]],
  n_raters = gstudy$design_levels[["rater"]],
  n_occasions = gstudy$design_levels[["occasion"]]
) {
  if (!inherits(gstudy, "gstudy_gtheoryr") || is.null(gstudy$facet_labels) ||
      !identical(gstudy$facet_labels, c("item", "rater", "occasion"))) {
    stop("`gstudy` must come from gstudy_pxiro().", call. = FALSE)
  }

  dstudy_crossed(
    gstudy,
    design_levels = c(item = n_items, rater = n_raters, occasion = n_occasions)
  )
}

#' Variance Component Proportions from a G-study
#'
#' Returns the estimated variance components together with their proportion of
#' the sum of estimated variance components.
#'
#' @param x A `gstudy_gtheoryr` object.
#' @param ... Unused.
#'
#' @return A data frame.
#' @examples
#' scores <- expand.grid(
#'   person = c("P1", "P2", "P3"),
#'   item = c("I1", "I2"),
#'   rater = c("R1", "R2"),
#'   occasion = c("T1", "T2"),
#'   stringsAsFactors = FALSE
#' )
#' scores$score <- c(
#'   8, 8, 7, 7, 7, 7, 6, 6,
#'   5, 5, 4, 4, 6, 6, 5, 5,
#'   7, 7, 6, 6, 8, 8, 7, 7
#' )
#'
#' gs <- gstudy_pxiro(
#'   scores,
#'   person = "person",
#'   item = "item",
#'   rater = "rater",
#'   occasion = "occasion",
#'   score = "score"
#' )
#' variance_proportions_table(gs)
variance_proportions_table <- function(x, ...) {
  UseMethod("variance_proportions_table")
}

#' @export
variance_proportions_table.gstudy_gtheoryr <- function(x, ...) {
  total <- sum(x$variance_components)
  proportion <- if (isTRUE(all.equal(total, 0))) {
    rep(NA_real_, length(x$variance_components))
  } else {
    x$variance_components / total
  }
  data.frame(
    component = names(x$variance_components),
    estimate = unname(x$variance_components),
    proportion = unname(proportion),
    row.names = NULL
  )
}

#' Extract ANOVA Components from a G-study
#'
#' Returns a data frame of sums of squares and degrees of freedom from a
#' `gstudy_gtheoryr` object.
#'
#' @param x A `gstudy_gtheoryr` object.
#' @param ... Unused.
#'
#' @return A data frame.
#' @examples
#' scores <- data.frame(
#'   person = rep(c("P1", "P2", "P3"), each = 3),
#'   item = rep(c("I1", "I2", "I3"), times = 3),
#'   score = c(8, 7, 9, 5, 4, 6, 7, 6, 8)
#' )
#' gs <- gstudy_pxi(scores, person = "person", item = "item", score = "score")
#' anova_table(gs)
anova_table <- function(x, ...) {
  UseMethod("anova_table")
}

#' @export
anova_table.gstudy_gtheoryr <- function(x, ...) {
  gstudy_anova_table(x)
}

#' Extract Mean Squares from a G-study
#'
#' Returns a data frame of ANOVA mean squares from a `gstudy_gtheoryr` object.
#'
#' @param x A `gstudy_gtheoryr` object.
#' @param ... Unused.
#'
#' @return A data frame.
#' @examples
#' scores <- data.frame(
#'   person = rep(c("P1", "P2", "P3"), each = 3),
#'   item = rep(c("I1", "I2", "I3"), times = 3),
#'   score = c(8, 7, 9, 5, 4, 6, 7, 6, 8)
#' )
#' gs <- gstudy_pxi(scores, person = "person", item = "item", score = "score")
#' mean_squares_table(gs)
mean_squares_table <- function(x, ...) {
  UseMethod("mean_squares_table")
}

#' @export
mean_squares_table.gstudy_gtheoryr <- function(x, ...) {
  gstudy_mean_squares_table(x)
}

#' Extract Variance Components from a G-study
#'
#' Returns a data frame of estimated variance components from a
#' `gstudy_gtheoryr` object.
#'
#' @param x A `gstudy_gtheoryr` object.
#' @param ... Unused.
#'
#' @return A data frame.
#' @examples
#' scores <- data.frame(
#'   person = rep(c("P1", "P2", "P3"), each = 3),
#'   item = rep(c("I1", "I2", "I3"), times = 3),
#'   score = c(8, 7, 9, 5, 4, 6, 7, 6, 8)
#' )
#' gs <- gstudy_pxi(scores, person = "person", item = "item", score = "score")
#' variance_components_table(gs)
variance_components_table <- function(x, ...) {
  UseMethod("variance_components_table")
}

#' @export
variance_components_table.gstudy_gtheoryr <- function(x, ...) {
  gstudy_variance_components_table(x)
}

#' @export
print.gstudy_gtheoryr <- function(x, ...) {
  cat("Generalizability Study\n")
  cat("Design:", x$design, "\n")
  cat(format_design_counts(x$facet_counts), "\n\n")
  cat("ANOVA components\n")
  print(gstudy_anova_table(x))
  cat("\nMean squares\n")
  print(gstudy_mean_squares_table(x))
  cat("\nVariance components\n")
  print(gstudy_variance_components_table(x))
  invisible(x)
}

#' @export
print.dstudy_gtheoryr <- function(x, ...) {
  cat("Design Study\n")
  cat("Design:", x$design, "\n")
  cat(format_design_counts(x$design_counts), "\n\n")
  print(data.frame(
    quantity = c("relative_error", "absolute_error", "g_coefficient", "phi_coefficient"),
    estimate = c(x$relative_error, x$absolute_error, x$g_coefficient, x$phi_coefficient),
    row.names = NULL
  ))
  invisible(x)
}
