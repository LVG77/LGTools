library(dplyr)

band_members
band_instruments
band_instruments2

band_members %>% 
  mutate(tbl_src = "tbl_a") -> tbl_a

band_instruments2 %>% 
  mutate(tbl_src = "tbl_b") -> tbl_b

tbl_a %>% 
  full_join(tbl_b, by = c("name" = "artist")) %>% 
  mutate(tbl_src = case_when(
    !is.na(tbl_src.x) & is.na(tbl_src.y)        ~ "x only",
    is.na(tbl_src.x) & !is.na(tbl_src.y)        ~ "y only",
    !is.na(tbl_src.x) & !is.na(tbl_src.y)       ~ "both"
  )) %>% 
  select(-contains("tbl_src."))

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

full_join_with_src(band_members, band_instruments2, by = c("name" = "artist"))
full_join_with_src(band_members, band_instruments)
