
library(DBI)
library(dplyr)
library(purrr)
library(tidyr)

source("../../R/update_status.R")
source("../../R/CRUD_functions.R")
source("../../R/helper_fns.R")


drv <<- RSQLite::SQLite()
database <<- "../../sql/room_avail_test.db"
room_table0 <- loadData(database, 'new_room_status')

# create temp database
path_db <- tempfile()
file.copy(from = database, to = path_db, overwrite = TRUE)


test_that("update own room a status: single day", {
  
  # create update data
  use <- "write"
  room_no <- 1
  table <- 'new_room_status'
  date <- "2020-06-13"
  
  # none available
  am <- "neither"
  
  update_status(room_no,
                date,
                use,
                am,
                avail = NA,
                path_db)
  
  room_table1 <- loadData(path_db, 'new_room_status')
  
  avail_room_table1 <- room_table1[room_table1$Date == date, ]
  avail <-
    c(date, "Sat", room_no,
      'Unavailable','Unavailable','Unavailable',
      'Unavailable','Unavailable','Unavailable','Unavailable','Unavailable')
  
  expect_true(all(avail_room_table1 == avail))
  
  # morning available
  am <- "am"
  
  update_status(room_no,
                date,
                use,
                am,
                avail = NA,
                path_db)
  
  room_table1 <- loadData(path_db, 'new_room_status')
  
  avail_room_table1 <- room_table1[room_table1$Date == date, ]
  avail <-
    c(date, "Sat", room_no,
      'Available','Available','Available',
      'Unavailable','Unavailable','Unavailable','Unavailable','Unavailable')
  
  expect_true(all(avail_room_table1 == avail))
  
  # afternoon available
  am <- "pm"
  
  update_status(room_no,
                date,
                use,
                am,
                avail = NA,
                path_db)
  
  room_table1 <- loadData(path_db, 'new_room_status')
  
  avail_room_table1 <- room_table1[room_table1$Date == date, ]
  avail <-
    c(date, "Sat", room_no,
      'Unavailable','Unavailable','Unavailable',
      'Available','Available','Available','Available','Available')
  
  expect_true(all(avail_room_table1 == avail))
  
  # whole day available
  am <- "both"
  
  update_status(room_no,
                date,
                use,
                am,
                avail = NA,
                path_db)
  
  room_table1 <- loadData(path_db, 'new_room_status')
  
  avail_room_table1 <- room_table1[room_table1$Date == date, ]
  avail <-
    c(date, "Sat", room_no,
      'Available','Available','Available',
      'Available','Available','Available','Available','Available')
  
  expect_true(all(avail_room_table1 == avail))
})


test_that("update own room a status: multiple days", {
    
  # create update data
  use <- "write"
  room_no <- 1
  table <- 'new_room_status'
  date <- c("2020-03-12", "2020-03-13")
  
  am <- c("am", "both")
  
  update_status(room_no,
                date,
                use,
                am,
                avail = NA,
                path_db)
  
  room_table1 <- loadData(path_db, 'new_room_status')
  
  avail_room_table1 <- room_table1[room_table1$Date == date, ]
  
  avail <- rbind(
    c(date[1], "Thu", room_no, "Available", "Available", "Available",
      "Unavailable", "Unavailable", "Unavailable", "Unavailable", "Unavailable"),
    c(date[2], "Fri", room_no, "Available", "Available", "Available",
      "Available",   "Available",   "Available",   "Available",   "Available"))
  
  expect_true(all(avail_room_table1 == avail))
  
  #
  am <- c("both", "am")
  
  update_status(room_no,
                date,
                use,
                am,
                avail = NA,
                path_db)
  
  room_table1 <- loadData(path_db, 'new_room_status')
  
  avail_room_table1 <- room_table1[room_table1$Date == date, ]
  
  avail <- rbind(
    c(date[1], "Thu", room_no, "Available", "Available", "Available",
      "Available", "Available", "Available", "Available", "Available"),
    c(date[2], "Fri", room_no, "Available", "Available", "Available",
      "Unavailable", "Unavailable", "Unavailable", "Unavailable", "Unavailable"))
  
  expect_true(all(avail_room_table1 == avail))
  
  #
  am <- c("pm", "pm")
  
  update_status(room_no,
                date,
                use,
                am,
                avail = NA,
                path_db)
  
  room_table1 <- loadData(path_db, 'new_room_status')
  
  avail_room_table1 <- room_table1[room_table1$Date == date, ]
  
  avail <- rbind(
    c(date[1], "Thu", room_no, "Unavailable", "Unavailable", "Unavailable",
      "Available", "Available", "Available", "Available", "Available"),
    c(date[2], "Fri", room_no, "Unavailable", "Unavailable", "Unavailable",
      "Available", "Available", "Available", "Available", "Available"))
  
  expect_true(all(avail_room_table1 == avail))
})


test_that("update/book other room a status: single day", {
  
  times_headings <- c("9am_10am", "10am_11am", "11am_12pm",
                      "12pm_1pm", "1pm_2pm", "2pm_3pm", "3pm_4pm", "4pm_5pm")
  
  # create booking data
  use <- "booking"
  room_no <- 1
  table <- 'new_room_status'
  date <- "2020-06-13"
  
  # 9am_10am unavailable
  avail <-
    matrix(c('Unavailable','Available','Available',
             'Available','Available','Available','Available','Available'), nrow = 1)
  
  update_status(room_no,
                date,
                use,
                am = NA,
                avail,
                path_db)

  room_table1 <- loadData(path_db, 'new_room_status')

  avail_room_table1 <- room_table1[room_table1$Date == date, ]


  expect_true(all(avail_room_table1[, times_headings] == avail))
})
