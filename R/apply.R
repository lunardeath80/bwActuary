apply_chainladder <- function(x, lob_name,
                              assumptions) {
  cl_triangles <-
    list("ep", "wp", "paid", "inc", "reported_no", "settled_no") %>%
    setNames(c("ep", "wp", "paid", "inc", "reported_no", "settled_no"))

  apply_trig <- function(trig_name) {
    if (!is.null(assumptions[[trig_name]])) {
      do.call(chainladder,
              c(list(x$lobs[[lob_name]][[trig_name]]), assumptions[[trig_name]]))
    }
  }

  cl_triangles %>%
    lapply(apply_trig) %>%
    compact

}

apply_chainladder_all <- function(x, lob_assumptions) {

  map2(lob_assumptions, names(lob_assumptions),
       ~apply_chainladder(x,.y,.x))

}


default_assumptions <- list(
  ep = NULL,
  wp = list(development = 1),
  paid = list(),
  os = NULL,
  inc = list(),
  reported_no = list(),
  settled_no = list()

)


lob_assumptions <- list(
  "Personal Motor" = default_assumptions
)


cl_result <-
  apply_chainladder(x, "Personal Motor", default_assumptions)
