greedy_knapsack <- function(x, W) {
  # Validates the inputs
  stopifnot(
    "`W` must be a positive number!" = is.numeric(W) && length(W) == 1 && W > 0,
    "`x` must be a data.frame with columns `v` and `w`!" =
      is.data.frame(x) && all(c("v", "w") %in% names(x)),
    "`x` columns `v` and `w` must contain positive numbers only!" =
      is.numeric(x$v) && all(x$v > 0) && is.numeric(x$w) && all(x$w > 0)
  )

  # Calculates value density column
  x$v_density <- x$v / x$w

  # Sorts the data.frame x according to the v_density values, largest first
  ord <- order(x$v_density, decreasing = TRUE)
  x_sorted <- x[ord, ]

  # Sets w_cum (cummulative weight) and v_cum (cummulative value) to zero
  w_cum <- 0
  v_cum <- 0

  # Initializes iterator i to 1
  i <- 1

  # Enters the while loop only if the first element does not exceed W
  if (x_sorted[i, "w"] <= W) {
    # Iterates over each row in x to add the next most densely valuable element
    repeat {
      # Breaks if adding the next element exceeds W
      if (i > nrow(x) || w_cum + x_sorted[i, "w"] > W) break

      # Sets the new cummulative values with the current element
      w_cum <- w_cum + x_sorted[i, "w"]
      v_cum <- v_cum + x_sorted[i, "v"]
      i <- i + 1
    }
  }

  # Returns a list with the best value and elements found
  list(
    value = v_cum,
    elements = ord[seq_len(i - 1)]
  )
}