
library(testthat)
library(shiny)
library(DBI)
library(dplyr)
library(purrr)
library(tidyr)

source("../../R/CRUD_functions.R")

drv <<- RSQLite::SQLite()
database <<- "../../sql/room_avail_test.db"
room_table0 <- loadData(database, 'room_booked')

# create temp database
path_db <- tempfile()
file.copy(from = database, to = path_db, overwrite = TRUE)

credentials <-
  list(user_auth = TRUE,
       info = data.frame(
         ID = "Simon",
         Permissions = "standard"))

test_that("update room booking", {
  
  # create booking data
  dates_to_book <- "2020-06-13"
  booker_id <- credentials$info$ID
  all_table_cells_selected <-
    matrix(c(1, 6,
             1, 7),
           ncol = 2, byrow = TRUE)
  
  update_booking(room_no = 1,
                 booker = booker_id,
                 date = dates_to_book,
                 time_idx = all_table_cells_selected,
                 database = path_db,
                 table = "room_booked")
  
  room_table1 <- loadData(path_db, 'room_booked')
  
  expect_equivalent(
    room_table1,
    data.frame(booking_no  = c(1,2),
               date = c("2020-06-13", "2020-06-13"),
               time  = c("11am_12pm", "12pm_1pm"),
               room_no = c(1,1),
               booker = "Simon"))
})
