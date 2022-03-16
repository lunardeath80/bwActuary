run_project <- function(x){

  x$data_checks <- data_checks(x)
  x$chainladder <- apply_chainladder_all(x, lob_assumptions)

}

run_lob <- function(x, lob_name) {
  x$data_checks <- data_checks(x)
}
