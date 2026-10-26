# =====================================================================
# TerraNova Engineering Lab - Cool R Functions Showcase
# =====================================================================

#' 1. Robust Outlier Smoothing via Median Absolute Deviation (MAD)
#' Detects and smooths outliers by capping them at robust z-score thresholds.
smooth_outliers <- function(x, threshold = 3.0) {
  med <- median(x, na.rm = TRUE)
  mad_val <- mad(x, na.rm = TRUE)
  if (mad_val == 0) return(x)
  
  modified_z <- 0.6745 * (x - med) / mad_val
  x_capped <- x
  x_capped[modified_z > threshold] <- med + (threshold * mad_val / 0.6745)
  x_capped[modified_z < -threshold] <- med - (threshold * mad_val / 0.6745)
  
  message(sprintf("Smoothed %d outlier(s) out of %d observations.", 
                  sum(abs(modified_z) > threshold, na.rm = TRUE), length(x)))
  return(x_capped)
}

#' 2. Non-Parametric Bootstrap Confidence Interval Calculator
#' Computes bootstrap confidence intervals for any statistic function.
bootstrap_ci <- function(data, stat_func = mean, R = 2000, conf_level = 0.95) {
  n <- length(data)
  boot_stats <- replicate(R, {
    resample <- sample(data, size = n, replace = TRUE)
    stat_func(resample)
  })
  
  alpha <- 1 - conf_level
  ci <- quantile(boot_stats, probs = c(alpha / 2, 1 - alpha / 2), na.rm = TRUE)
  
  list(
    observed = stat_func(data),
    ci_lower = ci[1],
    ci_upper = ci[2],
    conf_level = conf_level,
    replicates = boot_stats
  )
}

#' 3. ASCII Matrix Visualizer
#' Prints a compact ASCII heatmap representation of a numeric matrix to the console.
ascii_matrix <- function(mat, palette = c(".", ":", "+", "*", "#", "@")) {
  mat_norm <- (mat - min(mat)) / (max(mat) - min(mat) + 1e-9)
  indices <- pmin(floor(mat_norm * (length(palette) - 1)) + 1, length(palette))
  
  char_mat <- matrix(palette[indices], nrow = nrow(mat))
  apply(char_mat, 1, function(row) cat(paste(row, collapse = ""), "\n"))
  invisible(char_mat)
}
