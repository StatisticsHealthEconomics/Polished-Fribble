
library(testthat)
library(shiny)

source("../R/cancelBooking.R")

credentials <-
  list(user_auth = TRUE,
       info = data.frame(
         ID = "Simon",
         Permissions = "standard"))

user_data <- credentials$info


testServer(
  cancelBookingServer,
  args = list(credentials = reactive({credentials}),
              user_data = reactive({user_data})), {
                
                session$setInputs(booking_no = "",
                                  cancel = 1)
                
                print(output$cancel_table)
                # expect_equal()
})

