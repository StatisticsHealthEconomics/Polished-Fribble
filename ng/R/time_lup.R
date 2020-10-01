
# time in hours not index
#
time_lup <- function(index) {
  
  inp <-
    index %>% 
    as_tibble(.name_repair = "unique") %>% 
    rename(date_row = '...1' ,
           time_col = '...2') %>% 
    mutate_all(as.character)
  
  slots_9to5 <-
    c("9am_10am", "10am_11am", "11am_12pm", "12pm_1pm",
      "1pm_2pm", "2pm_3pm", "3pm_4pm", "4pm_5pm") %>% 
    setNames(4:11)
  
  time_slots <- slots_9to5[inp$time_col]
  
  split(time_slots, f = inp$date_row)
}