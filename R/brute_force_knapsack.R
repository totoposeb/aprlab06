

#' Brute Force Knapsack Solver
#'
#' Solves the 0/1 knapsack problem by exhaustively enumerating all possible
#' subsets of items. The function iterates over every combination from 1 to
#' \code{2^n - 1}, checks whether the total weight of each combination is
#' within the capacity limit \code{W}, and keeps track of the combination
#' with the highest total value.
#'
#' @param x A \code{data.frame} that must contain the following two columns:
#'   \describe{
#'     \item{v}{A numeric vector of item values. All values must be positive.}
#'     \item{w}{A numeric vector of item weights. All weights must be positive.}
#'   }
#'   In addition, \code{x} must not have more than 31 rows, since the number
#'   of combinations grows exponentially with the number of items.
#'
#' @param W A single positive number giving the maximum weight capacity of the
#'   knapsack.
#'
#' @param parallel A logical value indicating whether the computation should
#'   be performed in parallel. Defaults to \code{FALSE}. When \code{TRUE}, the
#'   search space is split into pieces across multiple cores using
#'   \code{parallel::parLapply}.
#'
#' @return A list with the following two elements:
#'   \describe{
#'     \item{value}{A numeric scalar giving the maximum total value found.}
#'     \item{elements}{An integer vector giving the row indices of the items
#'       that make up the optimal solution. If no items are selected, this is
#'       an integer vector of length 0.}
#'   }
#'   For an empty input (\code{nrow(x) == 0}), the function returns
#'   \code{list(value = 0, elements = integer(0))}.
#'
#' @details
#' The function uses \code{intToBits} to convert an integer into a binary bit
#' vector, which represents a subset of items. For \code{n} items there are
#' \code{2^n - 1} non-empty subsets to examine, so the time complexity is
#' \eqn{O(2^n \cdot n)}. This makes the approach feasible only for a small
#' number of items (no more than 31).
#'
#' When \code{parallel = TRUE}, the total number of combinations is divided
#' into roughly equal pieces, one per available core. The number of cores is
#' determined by \code{parallel::detectCores()}, but is capped at 2 when the
#' environment variable \code{_R_CHECK_LIMIT_CORES_} is set (as is the case
#' during CRAN checks). The number of cores is further limited by the total
#' number of combinations. A cluster is created with
#' \code{parallel::makeCluster} and is always shut down on exit via
#' \code{on.exit}.
#'
#' @examples
#' x <- data.frame(
#'   v = c(10, 20, 30, 40),
#'   w = c(1, 2, 3, 4)
#' )
#' brute_force_knapsack(x, W = 5)
#' brute_force_knapsack(x, W = 5, parallel = TRUE)
#'
#' @export
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
