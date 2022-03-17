apply_selection <- function(x, selections = NULL) {

  base_table <- x$cl_summary %>%
    mutate(
      Method = case_when(
        Inc_Dev >= 0.95 ~ "Inc",
        T ~ "Inc_CL"
      ),

      Selected_Ultiamte = case_when(
        Method == "Inc_CL" ~ Inc_CL,
        Method == "Paid_CL" ~ Paid_CL,
        Method == "Inc" ~ Inc,
        Method == "Paid" ~ Paid,
        T ~ NA_real_
      )
    )
}

