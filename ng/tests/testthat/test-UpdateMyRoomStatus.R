
library(testthat)
library(shiny)

source("../R/updateMyRoomStatus.R")

credentials <-
  list(user_auth = TRUE,
       info = data.frame(
         ID = "Simon",
         Password = "Password1",
         Password_Hash = "$7$C6..../....9w4R0ATbf4zrF9YWFgSqsb4jCfGZ4hJkBRaTHdgjC43$Zjl3B3ohoW9q7Zoc9sMUe1kHwVzyiRgSFD6fGfthID2",
         Permissions = "standard"))

user_data <- credentials$info


testServer(
  updateMyRoomStatusServer,
  args = list(credentials = reactive({credentials}),
              user_data = reactive({user_data})), {
                
                session$setInputs(date_update = "2020-06-13",
                                  time = "am",
                                  save_room = 1)
                
                print(output$personal_table)
                # expect_equal()
              })

