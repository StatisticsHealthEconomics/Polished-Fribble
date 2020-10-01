
# from the table of possible bookings
# find rooms with matching criteria
# and change status
#
create_candidate_table <- function(input,
                                   user_ID) {
  
  # no times selected
  if (is.null(input$all_table_cells_selected)) return(NULL)
  if (dim(input$all_table_cells_selected)[2] != 2) return(NULL)
  if (any(is.na(input$all_table_cells_selected))) return(NULL)
  
  room_table <- loadData(database, 'new_room_status')
  indiv_table <- loadData(database, 'individual_information')
  
  table_shown <- tableShown(input$checkbox_ampm,
                            input$date_search,
                            user_ID)
  
  index  <- input$all_table_cells_selected
  row_id <- input$all_table_cells_selected[, 1]
  col_id <- input$all_table_cells_selected[, 2]
  
  time_slots <- time_lup(index)
  num_dates <- as.vector(unique(row_id))
  
  booking_cand <- list()
  
  for (j in seq_along(num_dates)) {
    row_idx <- num_dates[j]
    
    booking_cand[[j]] <-
      list(Date = table_shown[row_idx, "Date"],
           Weekday = table_shown[row_idx, "Weekday"],
           Room_no = table_shown[row_idx, "Room_no"],
           time_slots = time_slots[[j]])
  }
  
  ## all times available for each date?
  interval_avail <- NULL
  
  for (i in seq_along(num_dates)) {
    
    t <- time_slots[[i]]
    
    dat <- apply(as.matrix(room_table[, t] == "Available"),
                 MARGIN = 1,
                 FUN = all)
    
    interval_avail <- rbind(interval_avail, dat)
  }
  
  my_room_no <- indiv_table$RoomNumber[indiv_table$UserName == user_ID]
  
  candidate <- NULL
  
  for (i in seq_along(num_dates)) {
    candidate <-
      rbind(candidate,
            room_table[
              interval_avail[i, ] &
                room_table$Date == as.character(booking_cand[[i]]$Date) &
                room_table$Room_no != my_room_no &
                room_table$Room_no == booking_cand[[i]]$Room_no, ])
  }
  
  # change status to booked
  if (nrow(candidate) > 0) {
    for (i in seq_along(num_dates)) {
      candidate[i, time_slots[[i]]] <- "Booked"
    }
  }
  
  candidate
}
