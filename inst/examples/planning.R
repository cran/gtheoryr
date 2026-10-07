library(gtheoryr)

# Synthetic continuous scores, not a real assessment dataset.
d <- simulate_gstudy(
  30, c(item = 6, rater = 3),
  c(person = 4, item = 0.4, rater = 0.2,
    "person:item" = 0.8, "person:rater" = 0.3,
    "item:rater" = 0.1, residual = 1.5),
  mean = 50, seed = 2026
)
check_gstudy_design(d, "person", c("item", "rater"), "score")
gs <- gstudy_crossed(d, "person", c("item", "rater"), "score")
variance_components_table(gs)

# Truncation is explicit: inspect the returned adjusted_components attribute.
eb <- error_budget(gs, negative = "zero")
print(eb)
print(attr(eb, "adjusted_components"))
sem_gtheory(gs, negative = "zero")
score_interval(gs, scores = c(45, 50, 55), negative = "zero")

candidate_counts <- list(item = 4:12, rater = 2:5)
grid <- dstudy_grid(gs, candidate_counts, negative = "zero")
plan <- optimize_dstudy(
  gs, candidate_counts, target = 0.85, coefficient = "phi",
  costs = c(item = 1, rater = 5), observation_cost = 0.5,
  negative = "zero"
)
print(plan$best)
dstudy_sensitivity(gs, negative = "zero")

# A short demonstration; increase B to 1000 or more for substantive analysis.
boot <- bootstrap_gstudy(gs, B = 50, seed = 123, negative = "zero")
print(boot$intervals)
