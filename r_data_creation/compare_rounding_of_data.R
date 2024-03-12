rounding_check <- function(df, digs){
  orig_rows <- nrow(df)
  new_rows <- df %>% mutate(check = round(tons_2017, digits = digs)+
                              round(tons_2022, digits = digs)+
                              round(value_2017, digits = digs)+
                              round(value_2022, digits = digs)) %>% 
    filter(check > 0) %>% nrow()
  
  return((orig_rows - new_rows)/orig_rows)
}

size_check <- function(df, digs){
  
  new_rows <- df %>% mutate(check = round(tons_2017, digits = digs)+
                              round(tons_2022, digits = digs)+
                              round(value_2017, digits = digs)+
                              round(value_2022, digits = digs)) %>% 
    filter(check > 0) %>% 
    select(-check) %>%
    object.size()
  return(new_rows)
}

rounding_check(dat, 3)
rounding_check(dat_cs, 3)
rounding_check(dat_ss, 3)

rounding_check(dat, 4)
rounding_check(dat_cs, 4)
rounding_check(dat_ss, 4)

rounding_check(dat, 5)
rounding_check(dat_cs, 5)
rounding_check(dat_ss, 5)

rounding_check(dat, 6)
rounding_check(dat_cs, 6)
rounding_check(dat_ss, 6)

size_check(dat, 3)
size_check(dat_cs, 3)
size_check(dat_ss, 3)

size_check(dat, 4)
size_check(dat_cs, 4)
size_check(dat_ss, 4)

size_check(dat, 5)
size_check(dat_cs, 5)
size_check(dat_ss, 5)

size_check(dat, 6)
size_check(dat_cs, 6)
size_check(dat_ss, 6)
