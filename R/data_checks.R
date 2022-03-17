data_checks_all <- function(x) {

  map(x$lobs, check_trig_dep)
}

data_checks <- function(lob) {
  check_trig_dep(lob)
}

check_trig_dep <- function(lob) {

  test_names <- rep("Fail",4) %>%
    as.list() %>%
    setNames(c("Paid+OS=Inc", "EP<WP", "Paid<0", "Inc<0"))

  if (all(lob$paid + lob$os == lob$inc, na.rm = T)) {
    test_names$`Paid+OS=Inc` <- "Pass"
  }

  if (all(lob$ep <= lob$wp, na.rm = T)) {
    test_names$`EP<WP` <- "Pass"
  }

  if(all(lob$Paid >= 0, na.rm = T)) {
    test_names$`Paid<0` <- "Pass"
  }

  if (all(lob$inc >= 0, na.rm = T)) {
    test_names$`Inc<0` <- "Pass"
  }

  test_names %>%
    as_tibble()

}
