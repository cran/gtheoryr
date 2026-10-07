test_that("gstudy_pxi and dstudy_pxi work on a balanced crossed design", {
  scores = data.frame(
    person = rep(c("P1", "P2", "P3"), each = 3),
    item = rep(c("I1", "I2", "I3"), times = 3),
    score = c(8, 7, 9, 5, 4, 6, 7, 6, 8)
  )

  gs = gstudy_pxi(scores, person = "person", item = "item", score = "score")
  ds = dstudy_pxi(gs)

  expect_s3_class(gs, "gstudy_gtheoryr")
  expect_s3_class(ds, "dstudy_gtheoryr")
  expect_true(all(c("person", "item", "residual") %in% names(gs$variance_components)))
  expect_true(all(c("relative_error", "absolute_error", "g_coefficient", "phi_coefficient") %in% names(ds)))
})

test_that("gstudy_pxif and dstudy_pxif support a third crossed facet", {
  scores = expand.grid(
    person = c("P1", "P2", "P3", "P4"),
    item = c("I1", "I2", "I3"),
    rater = c("R1", "R2"),
    stringsAsFactors = FALSE
  )

  base_person = c(P1 = 3, P2 = -1, P3 = 2, P4 = -2)
  base_item = c(I1 = 0.5, I2 = -0.5, I3 = 0)
  base_rater = c(R1 = 0.3, R2 = -0.3)
  interaction_noise = c(-0.2, 0.1, 0.0, 0.2)

  scores$score = 50 +
    base_person[scores$person] +
    base_item[scores$item] +
    base_rater[scores$rater] +
    rep(interaction_noise, length.out = nrow(scores))

  gs = gstudy_pxif(
    scores,
    person = "person",
    item = "item",
    facet = "rater",
    score = "score",
    facet_name = "rater"
  )
  ds = dstudy_pxif(gs)

  expect_s3_class(gs, "gstudy_gtheoryr")
  expect_s3_class(ds, "dstudy_gtheoryr")
  expect_true(all(c("person", "item", "rater", "person:item", "person:rater", "item:rater", "residual") %in% names(gs$variance_components)))
  expect_equal(gs$n_persons, 4)
  expect_equal(gs$n_items, 3)
  expect_equal(gs$n_facets, 2)
  expect_true(is.finite(ds$g_coefficient))
  expect_true(is.finite(ds$phi_coefficient))
})

test_that("extractor helpers return data frames", {
  scores = data.frame(
    person = rep(c("P1", "P2", "P3"), each = 3),
    item = rep(c("I1", "I2", "I3"), times = 3),
    score = c(8, 7, 9, 5, 4, 6, 7, 6, 8)
  )

  gs = gstudy_pxi(scores, person = "person", item = "item", score = "score")

  expect_s3_class(anova_table(gs), "data.frame")
  expect_s3_class(mean_squares_table(gs), "data.frame")
  expect_s3_class(variance_components_table(gs), "data.frame")
})

test_that("generic crossed engine supports raters and occasions together", {
  scores = expand.grid(
    person = c("P1", "P2", "P3"),
    item = c("I1", "I2"),
    rater = c("R1", "R2"),
    occasion = c("T1", "T2"),
    stringsAsFactors = FALSE
  )

  scores$score = c(
    8.3, 8.0, 7.7, 7.5, 7.4, 7.2, 6.8, 6.7,
    5.3, 5.0, 4.8, 4.6, 4.4, 4.1, 3.9, 3.8,
    7.1, 7.0, 6.6, 6.4, 6.3, 6.1, 5.7, 5.6
  )

  gs = gstudy_pxiro(
    scores,
    person = "person",
    item = "item",
    rater = "rater",
    occasion = "occasion",
    score = "score"
  )
  ds = dstudy_pxiro(gs)

  expect_s3_class(gs, "gstudy_gtheoryr")
  expect_s3_class(ds, "dstudy_gtheoryr")
  expect_true(all(c(
    "person", "item", "rater", "occasion",
    "person:item", "person:rater", "person:occasion",
    "item:rater", "item:occasion", "rater:occasion",
    "person:item:rater", "person:item:occasion",
    "person:rater:occasion", "item:rater:occasion",
    "residual"
  ) %in% names(gs$variance_components)))
  expect_true(is.finite(ds$g_coefficient))
  expect_true(is.finite(ds$phi_coefficient))
  expect_s3_class(variance_proportions_table(gs), "data.frame")
})
