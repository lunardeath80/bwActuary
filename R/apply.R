apply_chainladder <- function(x, lob_name,
                              assumptions) {
  cl_trig_names <-
    list("ep", "wp", "paid", "inc", "reported_no", "settled_no") %>%
    setNames(c("ep", "wp", "paid", "inc", "reported_no", "settled_no"))

  apply_trig <- function(trig_name) {
    if (!is.null(assumptions[[trig_name]])) {
      do.call(chainladder,
              c(list(x$lobs[[lob_name]][[trig_name]]), assumptions[[trig_name]]))
    }
  }

  cl_trigs <- cl_trig_names %>%
    lapply(apply_trig) %>%
    compact

}

apply_chainladder_all <- function(x, lob_assumptions) {

  results <- map2(lob_assumptions, names(lob_assumptions),
       ~apply_chainladder(x,.y,.x))

  summarise_results <- function(lob, lob_name){
    tibble(
      Paid = lob$paid$result$Latest,
      OS = lob$os$result$Latest,
      Inc = lob$os$result$Latest,
      Paid_Dev = lob$paid$result$Development,
      Paid_CL = lob$paid$result$Ultimate,
      Inc_Dev = lob$inc$result$Development,
      Inc_CL = lob$inc$result$Ultimate,
    ) %>%
      mutate(LoB = lob_name, .before = "Paid")
  }

  cl_summary <- results %>%
    map2(names(.),summarise_results) %>%
    do.call(bind_rows,.)

  return(list("results" = results, "cl_summary" = cl_summary))

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


