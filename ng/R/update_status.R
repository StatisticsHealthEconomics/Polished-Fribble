
# Update status
#
# update rows in table "new_room_status" from database "room_avail"
#
#   use = "write", function inputs am="am"/"pm"/"both"/"neither", used for writing personal room status to the table
#   use = "booking", the function inputs avail
#
# avail is an n by 8 matrix, each row corresponds to 8 periods for certain room on a specific day
#
# new_room_status is a table created in MySQL with Room_no and Date as PRIMARY,
# so that when updating rows with the same Room_no and Date existing in the table
# it modifies the existing row rather than adding a new row
#
update_status <- function(room_no,
                          date,
                          use = "write",
                          am = NA,
                          avail = NA,
                          database,
                          table) {
  
  # contain in speechmarks and paste together
  ## dbQuoteLiteral() use?
  parse_query <- function(dat) {
    
    dat %>% 
      mutate(date = paste0(date, "'"),
             weekday = paste0("'", weekday, "'"),
             room_no = paste0("'", room_no, "'"),
             avail = paste0("'", map(avail, paste, collapse = "','"))) %>% 
      tidyr::unite("q", date:avail, sep = ",") %>% 
      select(q) %>% 
      unlist()
  }
  
  time_slots <- c("9am_10am", "10am_11am", "11am_12pm", "12pm_1pm",
                  "1pm_2pm", "2pm_3pm", "3pm_4pm", "4pm_5pm")
  
  if (use == "write") {
    
    A <- tibble(date = date,
                weekday = date_to_weekday(date),
                room_no = room_no,
                am = am,
                avail = case_when(
                  am == "am" ~ list(c(rep("Available",3), rep("Unavailable",5))),
                  am == "pm" ~ list(c(rep("Unavailable",3), rep("Available",5))),
                  am == "both" ~ list(c(rep("Available",3), rep("Available",5))),
                  am == "neither" ~ list(c(rep("Unavailable",3), rep("Unavailable",5)))),
                ) %>% 
      select(-am)
    
    q <- parse_query(A)
    
  } else if (use == "booking") {
    
    A <- tibble(date = date,
                weekday = date_to_weekday(date),
                room_no = room_no,
                avail = list(avail[, time_slots]))
    
    q <- parse_query(A)
  }
  
  table_headings <- c("Weekday", "Room_no",
                      "9am_10am", "10am_11am", "11am_12pm", "12pm_1pm", "1pm_2pm", "2pm_3pm", "3pm_4pm", "4pm_5pm")

  # slightly different syntax
  
  ## MYSQL  
  # query <- paste(
  #   sprintf(
  #     "INSERT INTO %s VALUES ('%s')", table, q),
  #   "ON DUPLICATE KEY UPDATE",
  #   paste(sprintf("%1$s = VALUES(%1$s)", table_headings), collapse = ", "),
  #   collapse = "; ")
    
  ## SQLite
  query <- paste(
    sprintf(
      "INSERT INTO %s VALUES ('%s')", table, q),
    "ON CONFLICT (Date, Room_no) DO UPDATE SET ",
    paste(sprintf("'%1$s' = excluded.'%1$s'", table_headings), collapse = ", "))
  

  db <- dbConnect(drv, #MySQL(),
                  dbname = database,
                  host = options()$edwinyu$host, 
                  port = options()$edwinyu$port,
                  user = options()$edwinyu$user, 
                  password = options()$edwinyu$password)
  on.exit(dbDisconnect(db))
  
  dbGetQuery(db, query)  #check.names=FALSE?
}

