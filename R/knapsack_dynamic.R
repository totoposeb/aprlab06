knapsack_dynamic <- function(x,W){
# 1. check
# An error. That covers an x that isn't a data frame, a missing column,
#a value or weight that isn't positive, and a W that is negative or isn't one number.
  stopifnot(
    "`W` must be a positive number!" = is.numeric(W) && length(W) == 1 && W > 0,
    "`x` must be a data.frame with columns `v` and `w`!" =
      is.data.frame(x) && all(c("v", "w") %in% names(x)),
    "`x` columns `v` and `w` must contain positive numbers only!" =
      is.numeric(x$v) && all(x$v > 0) && is.numeric(x$w) && all(x$w > 0)
  )


  n <- nrow(x) #caculate the total number
  W <- as.integer(W) #the limit

  # 2. bulid matrix
  m <- matrix(0, nrow = n + 1, ncol = W + 1)  #NA matrix
  w <- x$w
  v <- x$v
  # 3. fill with matrix
  for (i in seq_len(n)) {
    for (j in seq_len(W)) {
      if (w[i]> j) {
        m[i, j] <- m[i - 1, j]
      } else{
        m[i, j] <- max(m[i - 1, j], m[i - 1, (j - w[i])] + v[i])
      }
    }
  }

  # 4. choose the maximum
  best_value <- m[n + 1, W + 1]

  # 5. show all elements
  elements <- integer(0)
  j <- W
  for (i in n:1) {
    if (m[i + 1, j + 1] != m[i, j + 1]) {
      elements <- c(i, elements)
      j <- j - x$w[i]
    }
  }

  list(value = round(best_value), elements = elements)

}
