run_project <- function(x){

  x$data_checks <- data_checks(x)

  lob_assumptions <- as.character(c(1:10)) %>%
    as.list() %>%
    setNames(as.character(1:10)) %>%
    map(~default_assumptions)

  cl <- apply_chainladder_all(x, lob_assumptions)

}

run_lob <- function(x, lob_name) {
  x$data_checks <- data_checks(x)
}


#Stress Test
