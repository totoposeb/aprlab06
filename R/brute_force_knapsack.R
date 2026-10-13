brute_force_knapsack <- function(x, W, parallel = FALSE) {
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
    "`x` cannot exceed 31 rows!" = nrow(x) <= 31
  )

  # Gets the number of items
  n <- nrow(x)
  total_combos <- 2^n - 1

  # Returns 0 value and empty elements if x is empty
  if (n == 0) return(list(value = 0, elements = integer(0)))

  # Copies columns into vector to Optimize after profvis call
  w <- x[, "w"]
  v <- x[, "v"]

  search_piece <- function(piece, w, v, W) {
    # Sets the variables to store the best results so far
    v_max <- 0
    best_rows <- integer(0)

    # Iterates through all combinations in the piece
    for (i in piece[1]:piece[2]) {
      # Expresses the current combination in binary
      bin_subset <- intToBits(i)

      # Selects the items for the current combination
      selected_rows <- which(bin_subset == 1)

      # Checks if the current combination is within the W limits
      if (sum(w[selected_rows]) <= W) {
        # Calculates the total value of the current combination
        v_sum <- sum(v[selected_rows])

        # If the max value is larger than the current one, it trumps it
        best_rows <- if (v_sum > v_max) selected_rows else best_rows
        v_max <- if (v_sum > v_max) v_sum else v_max
      }
    }

    list(value = v_max, elements = best_rows)
  }

  # Checks if parallelization is called or not
  if(parallel) {
    # Detects number of cores
    n_cores <- parallel::detectCores()
    if (nzchar(Sys.getenv("_R_CHECK_LIMIT_CORES_"))) n_cores <- min(n_cores, 2)
    n_cores <- if (total_combos >= n_cores) n_cores else total_combos

    # Defines pieces according to total_combos and n_cores
    breaks <- round(seq(0, total_combos, length.out = n_cores + 1))
    pieces <- lapply(
      seq_len(length(breaks) - 1), \(k) c(breaks[k] + 1, breaks[k + 1])
    )

    # Creates one worker per core and closes it at the end
    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl))

    # Calls search_piece once per worker
    results <- parallel::parLapply(
      cl, pieces, search_piece, w = w, v = v, W = W
    )

    # Selects the best results from all parallel executions
    values <- sapply(results, function(r) r$value)
    results <- results[[which.max(values)]]
  } else {
    # Calls search_piece once
    piece <- c(1, total_combos)
    results <- search_piece(piece, w, v, W)
  }

  # Returns a list with the best value and elements found
  results
}
