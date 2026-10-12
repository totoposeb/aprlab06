knapsack_dynamic <- function(x, W) {
  # Validates the inputs
  stopifnot(
    "`W` must be a positive number!" = is.numeric(W) && length(W) == 1 && W > 0,
    "`x` must be a data.frame with columns `v` and `w`!" = is.data.frame(x) &&
      all(c("v", "w") %in% names(x)),
    "`x` columns `v` and `w` must contain positive numbers only!" = is.numeric(
      x$v
    ) &&
      all(x$v > 0) &&
      is.numeric(x$w) &&
      all(x$w > 0),
    "`W` and `x$w` must all be integers!" = all(x$w == round(x$w)) &&
      W == round(W)
  )

  n <- nrow(x) # caculate the total number
  W <- as.integer(W) # the limit

  # 2. bulid matrix
  m <- matrix(0, nrow = n + 1, ncol = W + 1) # zero matrix
  w <- x$w
  v <- x$v

  # 3. fill with matrix
  for (i in seq_len(n)) {
    # Fills the row of item i for every knapsack size j
    for (j in seq_len(W)) {
      if (w[i] > j) {
        # Copies the cell above when item i is too heavy for size j
        m[i + 1, j + 1] <- m[i, j + 1]
      } else {
        # Keeps the larger: leaving item i out or putting it in
        m[i + 1, j + 1] <- max(m[i, j + 1], m[i, j - w[i] + 1] + v[i])
      }
    }
  }

  # 4. choose the maximum
  best_value <- m[n + 1, W + 1]

  # 5. show all elements
  elements <- integer(0)
  j <- W
  for (i in rev(seq_len(n))) {
    # Checks if allowing item i changed the best value for size j
    if (m[i + 1, j + 1] != m[i, j + 1]) {
      # Stores item i in front of the ones found so far
      elements <- c(i, elements)

      # Takes the weight of item i off the size that is left
      j <- j - x$w[i]
    }
  }

  # Returns a list with the best value and elements found
  list(
    value = round(best_value),
    elements = elements
  )
}
