# rm(list = ls())

swap <- function(x, y) {
  env <- parent.frame()
  xs <- substitute(x); xval = x
  ys <- substitute(y); yval = y
  do.call("<-", list(xs, y), envir = env)
  do.call("<-", list(ys, x), envir = env)
  invisible()
}


is_sorted <- function(x, order = c("asc", "desc")) {
  order = match.arg(order)
  if (order == "desc") x = -x
  all(diff(x) >= 0)
}


bubble_sort_asc <- function(x) {
  n <- length(x)
  while (!is_sorted(x)) {
    for (i in seq_len(n - 1)) {
      if (x[i] > x[i + 1]) {
        swap(x[i], x[i + 1]) 
      }
    }
  }
  x
}


bubble_sort <- function(x, ..., order = c("asc", "desc")) {
  order = match.arg(order)
  if (order == "desc") {
    - bubble_sort_asc(- x)
  } else {
    bubble_sort_asc(x)
  }
}


# Tests -----

test_bubble_sort_asc <- function() {
  original <- c(3, 1, 4, 2)
  sorted <- c(1, 2, 3, 4)
  if (any(bubble_sort(original) != sorted)) {
    stop("bubble_sort(order = 'asc') is not working correctly")
  }
}

test_bubble_sort_desc <- function() {
  original <- c(3, 1, 4, 2)
  sorted <- c(4, 3, 2, 1)
  if (any(bubble_sort(original, order = "desc") != sorted)) {
    stop("bubble_sort(order = 'desc') is not working correctly")
  }
}


test_swap <- function() {
  x <- 1
  y <- 2
  swap(x, y)
  if (x != 2 || y != 1) {
    stop("swap is not working correctly.")
  }
}


test_bubble_sort_asc()
test_bubble_sort_desc()
test_swap()
