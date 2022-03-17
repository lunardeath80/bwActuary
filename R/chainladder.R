chainladder <-
  function(triangle,
           weights = NULL,
           ldf = NULL,
           development = NULL,
           tail = 1) {
    link_ratios <- triangle[-1] / triangle[-ncol(triangle)]

    if (is.null(development)) {
      development_assumptions <-
        calc_development(triangle, ldf, weights, tail)
      development <- development_assumptions$development
      ldf <- c(development_assumptions$ldf, tail)
      weights <- development_assumptions$weights
    }

    latest <- diagonal(triangle)
    ultimate <- latest / rev(development)

    list(
      result = data.frame(
        Latest = latest,
        Development = development,
        Ultimate = ultimate
      ),
      ldf = ldf,
      triangle = triangle,
      weights = weights
    )


  }

diagonal <- function(triangle, offset = 0) {
  if (offset != 0) {
    warning("Non-zero offset not yet supported, returning latest diagonal")
    offset <- 0
  }
  ratio <- ncol(triangle) / nrow(triangle)
  sel_cols <- seq(ncol(triangle), 1, by = -ratio)
  diag <- double(nrow(triangle))

  for (i in 1:nrow(triangle)) {
    diag[i] <- triangle[i, sel_cols[i]]
  }
  diag
}

calc_link_ratio <- function(triangle){
  triangle[,-1]/triangle[,-ncol(triangle)]
}
calc_development <-
  function(triangle,
           ldf = NULL,
           weights = NULL,
           tail = 1) {
    if (is.null(ldf)) {
      ldf_and_weights <- calc_ldf(triangle, weights)
      ldf <- ldf_and_weights$ldf
      weights <- ldf_and_weights$weights
    }

    cdf <- rev(cumprod(rev(c(ldf, tail))))
    ratio <- ncol(triangle) / nrow(triangle)

    development <- 1 / cdf[seq(length(cdf), 1, -ratio)]

    return(list(
      development = development,
      ldf = ldf,
      weights = weights
    ))

  }

calc_ldf <- function(triangle, weights) {
  if (is.null(weights)) {
    weights <- calc_weights(triangle)

  }

  weights2 <- (triangle / triangle)[, -1]
  ldf <- double(ncol(triangle) - 1)

  for (j in 2:ncol(triangle)) {
    ldf[j - 1] <-
      sum((triangle[, j] * weights[, j - 1]), na.rm = T) / sum(triangle[, j -
                                                                          1] * weights2[, j - 1], na.rm = T)
  }

  return(list(ldf = ldf, weights = weights))


}

calc_weights <- function(triangle) {
  link_ratios <- triangle[, -1] / triangle[, -ncol(triangle)]
  weights <- triangle / triangle

  return(weights)

  for (j in 1:ncol(link_ratios)) {
    sel_col <- link_ratios[, j]
    col_average <- mean(sel_col, na.rm = T)
    col_sd <- sd(sel_col, na.rm = T)

    col_max <- col_average + col_sd
    col_min <- col_average - col_sd

    weights[, j] <- (sel_col < col_max) * (sel_col < col_min)


  }

}
