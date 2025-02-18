create_project <-
  function(client_name,
           review_date,
           latest_diag_date) {
    x <- new.env()

    x$client_name <- client_name
    x$review_date <- review_date
    x$latest_diag_date <- latest_diag_date
    x$lobs <- list()
    return(x)
  }

add_lob <- function(x,
                    lob_name,
                    origin,
                    dev_ratio,
                    ep,
                    wp,
                    paid,
                    os = NULL,
                    inc = NULL,
                    reported_no,
                    settled_no) {
  x$lobs[[lob_name]] <- list()

  x$lobs[[lob_name]]$origin <- origin
  x$lobs[[lob_name]]$dev <- 1:(length(origin) * dev_ratio)




  if (is.null(os) && !is.null(paid) && !is.null(inc)) {
    os <- inc - paid
  }
  if (is.null(inc) && !is.null(paid) && !is.null(os)) {
    inc <- paid + os
  }

  x$lobs[[lob_name]]$ep <- ep
  x$lobs[[lob_name]]$wp <- wp
  x$lobs[[lob_name]]$paid <- paid
  x$lobs[[lob_name]]$os <- os
  x$lobs[[lob_name]]$inc <- inc
  x$lobs[[lob_name]]$reported_no <- reported_no
  x$lobs[[lob_name]]$settled_no <- settled_no
  return(x)


}

gen_trig <- function(origin,
                     ratio,
                     sev_mean = 100,
                     sev_sd = 25) {
  seq(origin * ratio, 1, -ratio) %>%
    lapply(
      FUN = function(.x) {
        c(rnorm(.x, sev_mean, sev_sd), rep(NA, origin * ratio - .x))
      }
    ) %>%
    do.call(rbind, .) %>%
    to_cumulative()
}


to_cumulative <- function(trig) {
  for (j in 2:ncol(trig)) {
    trig[, j] <- trig[, j - 1] + trig[, j]
  }

  trig
}
to_incremental <- function(trig) {
  for (j in ncol(trig):2) {
    trig[, j] <- trig[, j] - trig[, j - 1]
  }

  trig
}


