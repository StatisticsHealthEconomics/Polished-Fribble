
#
updateMyRoomStatusUI <- function(id) {
  
  ns <- NS(id)
  uiOutput(ns("room_status"))
}


# If a room has been booked by some else (after user first updating their information)
# The user cannot change the room status again,
# and a notification is shown for asking him to contact admin
#
# examine if the row user wants to update exists in the table
# if exists, checks if any time has been booked and then decide to update
# if doesn't exist, row is updated directly without checking
#
updateMyRoomStatusServer <- function(id, credentials) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      output$room_status <- renderUI({
        
        ns <- session$ns
        
        # req(credentials()$user_auth)
        
        fluidPage(
          box(width = 3,
              h4("Tell others when your room will be available for booking."),
              wellPanel(
                dateInput(ns('date_update'),
                          label = 'Date',
                          value = Sys.Date()) %>%
                  helper(colour = "mediumpurple1",
                         type = "inline",
                         size = "m",
                         title = "Guidance:",
                         content = c(
                           "- Input the date on which your rooms will be available for booking",
                           "- Then specify the exact period of the day: am, pm, both of them.",
                           "- After saving your chosen time slots,
                           it would be shown on the righthand side.",
                           "- You may always modify your available time slots,
                           so long as it isn't booked by someone else",
                           "- If you input nothing in this table,
                           your room will not appear in any search result
                           (unavailable by default)"))),
              
              checkboxGroupInput(ns("time"),
                                 "Time",
                                 choices = c("am","pm"),
                                 selected = ""),
              actionButton(ns("save_room"),
                           "Save/Refresh",
                           width = "25%")),
          box(width = 9,
              DT::dataTableOutput(outputId = ns('personal_table'))) %>%
            helper(
              colour = "mediumpurple1",
              type = "inline",
              size = "m",
              content = c("Explanation about cells:",
                          "",
                          "Available   :", "Your room can be booked for this time slot.",
                          "",
                          "Unavailable :", "Your room cannot be booked for this time slot.",
                          "",
                          "Booked      :", "Others have booked your room for this time slot.",
                          "You can no longer make changes on this day.",
                          "",
                          "To check for the availability of others' rooms and make bookings",
                          "please go to Room Booking.")),
          tags$head(
            tags$style(
              HTML(".shiny-notification {position:fixed;top: calc(50%);left: calc(50%);}")
            )
          ))
      })
      
      return(
        observeEvent(input$save_room, {   
          
          # req(credentials$user_auth)
          user_data <- credentials$info
          
          print(user_data)
          
          date_update <- input$date_update
          print(paste("date_update", date_update))
          
          avail_time <- paste(input$time, collapse = "")
          print(paste("avail_time", avail_time))
          
          ampm <- 
            switch(avail_time,
                   am = "am",
                   pm = "pm",
                   ampm = "both",
                   "neither")
          
          room_table <- loadData(database, 'new_room_status')
          indiv_table <- loadData(database, 'individual_information')
          
          my_room_no <-
            indiv_table$RoomNumber[indiv_table$UserName == user_data[['ID']]]
          print(paste("my_room_no", my_room_no)) 
          
          rows_existing <-
            (room_table$Room_no == my_room_no) &
            (room_table$Date == date_update)
          
          is_existing_record <- any(rows_existing)
          print(c("is_existing_record:", is_existing_record))
          
          print(c("rows_existing:", rows_existing))
          
          rooms_existing <- room_table[rows_existing, , drop = FALSE]
          print("rooms_existing:\n")
          print(rooms_existing)
          
          print(nrow(rooms_existing))
          
          if (nrow(rooms_existing) == 0) {
            is_room_booked <- FALSE
          } else {
            is_room_booked <- rooms_existing == "Booked"
          }
          print("is_room_booked:\n")
          print(is_room_booked)
          
          is_already_booked <- any(as.matrix(is_room_booked))
          print("is_already_booked:\n")
          print(is_already_booked)
          
          if (is_existing_record && is_already_booked) {
            
            showNotification("Someone has booked your room already.
                               Please contact admin.",
                             type = "error",
                             duration = 30,
                             closeButton = TRUE)
          } else {
            print("update_status()")
            update_status(use = "write",
                          room_no = my_room_no,
                          date = date_update,
                          am = ampm,
                          database = database,
                          table = 'new_room_status')
          }
          
          room_table <- loadData(database, 'new_room_status')
          personal_table <-
            room_table[room_table$Room_no == my_room_no &
                         !is_past(room_table$Date), ] %>% 
            arrange(desc(Date))
          
          print(paste("personal_table:", personal_table))
          
          output$personal_table <-
            DT::renderDataTable({
              personal_table},
              options = list(scrollX = TRUE,
                             # pageLength = 10
                             paging = FALSE)
            )
        })
      )
    }
  )
}

