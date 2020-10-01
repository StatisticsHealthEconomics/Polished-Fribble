
# load the table from the given database   
#
loadData <- function(database, table) {
  db <- dbConnect(drv,
                  dbname = database,
                  host = options()$edwinyu$host, 
                  port = options()$edwinyu$port,
                  user = options()$edwinyu$user, 
                  password = options()$edwinyu$password)
  on.exit(dbDisconnect(db), add = TRUE)
  
  query <- sprintf("SELECT * FROM %s", table)
  
  dbGetQuery(db, query)
}


# delete rows in table "new_room_status" from db "room_avail"
#
delete_status <- function(room_no,
                          date_1,
                          date_2 = date_1,
                          database,
                          table) {
  
  db <- dbConnect(drv,
                  dbname = database,
                  host = options()$edwinyu$host, 
                  port = options()$edwinyu$port,
                  user = options()$edwinyu$user, 
                  password = options()$edwinyu$password)
  on.exit(dbDisconnect(db), add = TRUE)
  
  query <- paste0('DELETE FROM ', table,
                  " WHERE Room_no = '", room_no,
                  "' AND ",
                  "Date >= '", date_1,"' AND <='", date_2,"'")
  
  print(query) #for debug
  dbSendQuery(db, query)
}


#' Update booking for table room_booked
#'
#' @param time_idx Matrix cols: row idx, col idx
#' @param booking_no Created internally if none provided
#'
update_booking <- function(room_no,
                           booker,
                           date,
                           time_idx,
                           booking_no = NA,
                           database,
                           table){
  db <- dbConnect(drv,
                  dbname = database,
                  host = options()$edwinyu$host, 
                  port = options()$edwinyu$port,
                  user = options()$edwinyu$user, 
                  password = options()$edwinyu$password)
  on.exit(dbDisconnect(db), add = TRUE)
  
  time_slots <- time_lup(time_idx)
  
  booking_no <- new_booking_no(booking_no, db, table, date)
  
  value_names <- c("booking_no", "date", "time", "room_no", "booker")
  
  q <- list()
  
  for (i in seq_along(date)) {
    
    # matrix of each row time slot
    time <- unlist(time_slots[[i]])
    values <- matrix(c(booking_no[i], date[i], room_no[i], booker), nrow = 1)
    dat <- values[rep(1, length(time)), , drop = FALSE]
    
    q[[i]] <- cbind(dat, time = time)
  }
  
  q <- do.call(rbind, q)
  
  # parse to query
  q <- 
    as_tibble(q, .name_repair = "minimal") %>%
    set_names(nm = c("booking_no", "date", "room_no", "booker", "time")) %>% 
    select(booking_no, date, time, room_no, booker) %>% # put in correct order
    mutate_at(vars(2:4), funs(paste0("'", ., "'"))) %>% 
    mutate_at(vars(1), funs(paste0(., "'"))) %>%        # don't quote first and last
    mutate_at(vars(5), funs(paste0("'", .))) %>%
    tidyr::unite("q", sep = ",") %>% 
    select(q) %>% 
    unlist()
  
  if (class(drv) == "MySQLDriver") {
    query <- paste(
      sprintf(
        "INSERT INTO %s VALUES ('%s')",
        table,
        paste(values, collapse = "', '")),
      "ON DUPLICATE KEY UPDATE",
      paste(sprintf("%1$s = VALUES(%1$s)", value_names), collapse = ", "))
    
  } else if (class(drv) == "SQLiteDriver") {
    
    ##TODO: primary key conflict. how to check on multiple columns...
    # query <- paste(
    #   paste0(
    #     sprintf(
    #       "INSERT INTO %s VALUES ('%s')", table, q),
    #     # " ON CONFLICT (date, time, room_no) DO UPDATE SET ",
    #     # paste(sprintf("'%1$s' = excluded.'%1$s'", value_names), collapse = ", "),
    #     ";"),
    #   collapse = " ")
    
    query <- 
      paste(
        sprintf("INSERT or IGNORE into %s VALUES", table),
        paste0("('", q, "')", collapse = ", "))
  }
  
  dbGetQuery(db, query)
}


# deleting booking from room_booked 
#
delete_booking <- function(booking_no,
                           database,
                           table) {
  db <- dbConnect(drv,
                  dbname = database,
                  host = options()$edwinyu$host, 
                  port = options()$edwinyu$port,
                  user = options()$edwinyu$user, 
                  password = options()$edwinyu$password)
  on.exit(dbDisconnect(db), add = TRUE)
  
  query <- sprintf("DELETE FROM %s WHERE booking_no = '%s'", table, booking_no)
  
  dbSendQuery(db, query)
}


# create unique booking ref
#
new_booking_no <- function(booking_no,
                           db,
                           table,
                           date) {
  
  if (!is.na(booking_no)) return(booking_no)
  
  max_booking_no <-
    dbGetQuery(
      db,
      sprintf("SELECT MAX(booking_no) FROM %s WHERE typeof(booking_no) = 'integer'",
              table)) %>% 
    unlist()
  
  if (length(max_booking_no) == 0 | is.na(max_booking_no))
    max_booking_no <- 0L
  
  max_booking_no + 1:length(date) 
}

