suppressWarnings(RNGversion(min(as.character(getRversion()),"3.5.3")))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <- data.frame(
  w=sample(1:4000, size = n, replace = TRUE),
  v=runif(n = n, 0, 10000)
)

test_that("Correct object is returned", {
  expect_silent(kd <- knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500))
  expect_named(kd, c("value", "elements"))
})


test_that("functions rejects errounous input.", {
  expect_error(knapsack_dynamic("hej", 3500))
  expect_error(knapsack_dynamic(x = knapsack_objects[1:8,], W = -3500))
})

test_that("Function return correct results.", {
  kd <- knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)
  expect_equal(round(kd$value), 16770)
  expect_true(all(round(kd$elements) %in% c(5, 8)))
  
  kd <- knapsack_dynamic(x = knapsack_objects[1:12,], W = 3500)
  expect_equal(round(kd$value), 16770)
  expect_true(all(round(kd$elements) %in% c(5, 8)))
  
  kd <- knapsack_dynamic(x = knapsack_objects[1:8,], W = 2000)
  expect_equal(round(kd$value), 15428)
  expect_true(all(round(kd$elements) %in% c(3, 8)))
  
  kd <- knapsack_dynamic(x = knapsack_objects[1:12,], W = 2000)
  expect_equal(round(kd$value), 15428)
  expect_true(all(round(kd$elements) %in% c(3, 8)))

  kd <- knapsack_dynamic(x = knapsack_objects[1:500,], W = 3500)
  expect_equal(round(kd$value), 164851)
  expect_true(sum(knapsack_objects$w[kd$elements]) <= 3500)
  expect_equal(sum(knapsack_objects$v[kd$elements]), kd$value)

  kd <- knapsack_dynamic(x = knapsack_objects[1:12,], W = 100)
  expect_equal(round(kd$value), 0)
  expect_length(kd$elements, 0)
  
  st <- system.time(kd <- knapsack_dynamic(x = knapsack_objects[1:16,], W = 2000))
  expect_true(as.numeric(st)[2] >= 0.00)
})

test_that("Function agrees with brute_force_knapsack.", {
  kd <- knapsack_dynamic(x = knapsack_objects[1:16,], W = 3500)
  bfk <- brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500)

  expect_equal(kd$value, bfk$value)
  expect_setequal(round(kd$elements), round(bfk$elements))
})

test_that("Function rejects weights with decimals.", {
  x <- data.frame(w = c(1.5, 2), v = c(1, 2))
  expect_error(knapsack_dynamic(x = x, W = 3), "integers")
})
