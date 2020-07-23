
date <- c("2020-06-12", "2020-06-13")
use <- "write"
room_no <- 1
am <- c("am", "both")
table <- 'new_room_status'
database <- 'room_avail'

avail <- setNames(data.frame('Unavailable','Unavailable','Unavailable',
                             'Available','Available','Available','Available','Available'), NULL)


update_status(use = use,
              room_no = room_no,
              date = date,
              am = am,
              database = 'room_avail',
              table = 'new_room_status')