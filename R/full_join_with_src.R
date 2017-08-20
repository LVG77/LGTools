
full_join_with_src <- function(df1, df2, by = NULL) {

  mutate(df1, tbl_src_x = "tbl_a") -> tbl_a
  mutate(df2, tbl_src_y = "tbl_b") -> tbl_b

  tbl_a %>%
    full_join(tbl_b, by = by) %>%
    mutate(tbl_src = case_when(
      !is.na(tbl_src_x) & is.na(tbl_src_y)        ~ "x only",
      is.na(tbl_src_x) & !is.na(tbl_src_y)        ~ "y only",
      !is.na(tbl_src_x) & !is.na(tbl_src_y)       ~ "both"
    )) %>%
    select(-contains("tbl_src_"))
}

