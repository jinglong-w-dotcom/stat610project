library(testthat)

# Test 1: Checking cluster center
test_that("computer_centers doesn't include NA and the cluster labels", {
  K <- 15
  n <- nrow(x)
  initial_clusters <- sample(1:K, size = n, replace = TRUE)
  centers <- compute_centers(x, initial_clusters, K)
  expect_equal(nrow(centers), K)
  expect_equal(ncol(centers), ncol(x))
  expect_true(!is.null(centers))
})

# Test 2: Normal conditions
test_that("k_means works under normal conditions", {
  K <- 15
  result <- k_means(x, K)
  
  expect_is(result, "list")
  expect_true(!is.null(result$centers))
  expect_true(!is.null(result$clusters))
  expect_equal(length(result$clusters), nrow(x))
  expect_equal(nrow(result$centers), K)
  
  # Validate the Within-Cluster Sum of Squares
  expect_true(is.numeric(result$total_wcss))
  expect_true(result$total_wcss[which.min(result$total_wcss)] > 0)
})

# Test 3: Number of clusters exceeds data points
test_that("k_means throws error when K > number of data points", {
  K <- nrow(x) + 1
  expect_error(k_means(x, K), "The number of cluster centers is greater than the number of data points.")
})

# Test 4: checking input data
test_that("k_means handles empty dataset", {
  empty_X <- matrix(numeric(0), nrow = 0, ncol = 2)
  expect_error(k_means(empty_X, 15), "The number of cluster centers is greater than the number of data points.")
})

test_that("k_means handles missing values", {
  X_with_na <- x
  X_with_na[sample(length(X_with_na), 10)] <- NA
  expect_error(k_means(X_with_na, 15), "Input dataset contains missing values.")
})

test_that("k_means throws error for non-numeric input", {
  non_numeric_X <- matrix(rep(letters, length.out = 100), ncol = 2)
  expect_error(k_means(non_numeric_X, 15), "Input dataset contains non-numeric values.")
})

# Test 5: Edge case with 2 clusters and 1 points
test_that("k_means works for minimal data", {
  X_minimal <- matrix(c(0, 0, 1, 1), ncol = 2)
  K <- 1
  result <- k_means(X_minimal, K)
  
  expect_equal(nrow(result$centers), K)
  expect_equal(length(unique(result$clusters)), K)
})

# Test 6: Progress visualization
test_that("K_means_progresses creates a GIF without errors", {
  K <- 3
  expect_silent(k_means(x, K))
  result <- k_means(x, K)
  expect_error(K_means_progresses(result, x), NA)
})

print("All tests passed!")


