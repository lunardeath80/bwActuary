
wp <- gen_trig(6, 4, 100, 5)
ep <- wp / 6
paid <- gen_trig(6, 4, 20, 10)
inc <- gen_trig(6, 4, 70, 10)
claim_reported <- gen_trig(6, 4, 1, 0.25) %>%
  round()
claim_settled <- round(claim_reported * 0.5)

x <-
  create_project("apple", "31/03/2021", "31/03/2021")

walk(
  as.character(1:42),
  ~ add_lob(
    x,
    .x,
    2009:2020,
    4,
    ep = ep,
    wp = wp,
    paid = paid,
    inc = inc,
    reported_no = claim_reported,
    settled_no = claim_settled
  )
)

run_project(x)


run_project <- function(x){

  x$data_checks <- data_checks(x)

  lob_assumptions <- as.character(c(1:10)) %>%
    as.list() %>%
    setNames(as.character(1:10)) %>%
    map(~default_assumptions)

  cl <- apply_chainladder_all(x, lob_assumptions)
  x$cl_summary <- cl$cl_summary
  x$cl_results <- cl$results
  x$results <- apply_selection(x)
}

run_lob <- function(x, lob_name) {
  x$data_checks <- data_checks(x)
}
