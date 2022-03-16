apply_chainladder <- function(x, lob_name,
                              assumptions) {
  cl_triangles <-
    c("ep", "wp", "paid", "inc", "reported_no", "settled_no")

  apply_trig <- function(trig_name) {
    if (!is.null(assumptions[[trig_name]])) {
      do.call(chainladder,
              c(list(x$lobs[[lob_name]][[trig_name]]), assumptions[[trig_name]]))
    }
  }

  cl_triangles %>%
    setNames(cl_triangles) %>%
    map(apply_trig)

}

default_assumptions <- list(
  ep = list(development = 1),
  wp = list(development = 1),
  paid = list(),
  os = NULL,
  inc = list(),
  reported_no = list(),
  settled_no = list()

)

