
#
room_confirm_msg <- function(num_dates,
                             room_no_to_book,
                             dates_to_book,
                             time_slots) {
  room_confirm <- NULL
  
  for (i in seq_along(num_dates)) {
    room_confirm <- paste(room_confirm,
                          'room', room_no_to_book[i], "\n",
                          "(", dates_to_book[i], ",",
                          date_to_weekday(dates_to_book[i]), ",",
                          paste(time_slots[[i]], collapse = ' and '),
                          ")", sep = " ")
  }
  
  paste("You have successfully booked", room_confirm)
}