brute_force_knapsack <- function(x, W) {
  # Validates the inputs
  stopifnot(
    "`W` must be a positive number!" = is.numeric(W) && length(W) == 1 && W > 0,
    "`x` must be a data.frame with columns `v` and `w`!" =
      is.data.frame(x) && all(c("v", "w") %in% names(x)),
    "`x` columns `v` and `w` must contain positive numbers only!" =
      is.numeric(x$v) && all(x$v > 0) && is.numeric(x$w) && all(x$w > 0),
    "`x` cannot exceed 31 rows!" = nrow(x) <= 31
  )

  # Gets the number of items
  n <- nrow(x)
  v_max <- 0
  best_rows <- c()

  # Iterates from 1 to 2^n, which is the total number of different combinations
  for (i in seq_len(2^n - 1)) {
    # Expresses the current combination in binary
    bin_subset <- intToBits(i)

    # Selects the items for the current combination
    selected_rows <- which(bin_subset == 1)

    # Checks if the current combination is within the W limits
    if (sum(x[selected_rows, "w"]) <= W) {
      # Calculates the total value of the current combination
      v_sum <- sum(x[selected_rows, "v"])

      # If the max value is larger than the current one, it trumps it
      best_rows <- if (v_sum > v_max) selected_rows else best_rows
      v_max <- if (v_sum > v_max) v_sum else v_max
    }
  }

  list(
    value = v_max,
    elements = best_rows
  )
}