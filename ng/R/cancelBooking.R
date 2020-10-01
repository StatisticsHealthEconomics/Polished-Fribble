
#
cancelBookingUI <- function(id) {
  
  ns <- NS(id)
  uiOutput(ns("cancel"))
}

#
cancelBookingServer <- function(id, credentials) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      output$cancel <- renderUI({
        
        ns <- session$ns
        
        # req(credentials$user_auth)

        fluidPage(
          box(width = 3,
              h4("Cancel the booked room"),
              textInput(ns("booking_no"),
                        "Input booking number here:",
                        value = "") %>%
                helper(
                  colour = "mediumpurple1",
                  type = "inline",
                  size = "m",
                  title = 'Guidance:',
                  content = c("Enter the booking number to the room that you no longer need.")),
              actionButton(ns("cancel"),
                           "Cancel/Refresh",
                           width = "50%"),
              tags$head(
                tags$style(
                  HTML(".shiny-notification {position:fixed;top: calc(50%);left: calc(50%);}")
                )
              )
          ),
          box(width = 9,
              dataTableOutput(outputId = ns('cancel_table')))
        )
      })
      
      return(
        observeEvent(input$cancel, {
          
          user_data <- credentials$info
          
          booked_table <- loadData(database, "room_booked")
          room_table <- loadData(database, 'new_room_status')
          indiv_table <- loadData(database, 'individual_information')
          
          if (length(input$booking_no) > 0 &&
              input$booking_no %in% booked_table$booking_no) {
            
            delete_info <-
              booked_table[booked_table$booking_no == input$booking_no, ]
            
            avail <-
              room_table %>%
              filter(Date == delete_info$date,
                     Room_no == delete_info$room_no) %>% 
              select(-Date, -Weekday, -Room_no)
            
            avail[1, delete_info$time] <- "Available"
            
            my_room_no <-
              indiv_table$RoomNumber[indiv_table$UserName == user_data[['ID']]]
            
            delete_booking(input$booking_no,
                           database = database,
                           table = "room_booked")
            
            print(paste("room_no:", delete_info$room_no))
            print(paste("date:", delete_info$date))
            print(paste("avail:", avail))
            
            update_status(use = "booking",
                          room_no = delete_info$room_no[1],
                          date = delete_info$date[1],
                          avail = avail,
                          database = database,
                          table = "new_room_status")
            
            ##TODO: is this the correct day?...
            
            current <- weekdays(as.POSIXct(Sys.Date()), abbreviate = FALSE)
            
            cancel_confirm_msg <-
              sprintf("You have successfully canceled the booked room %s \n (%s,%s,",
                      paste(input$time4, collapse = ' and '), ")",
                      input$room_book_no2, current, input$day4)

            ##TODO: finish/test this...            
            # sprintf("You have successfully canceled the booked room %s \n (%s,%s,%s)",
            #         paste(delete_info$times, collapse = ' and '),
            #         delete_info$room_no, delete_info$day, delete_info$date)

            showNotification(cancel_confirm_msg,
                             type = "message",
                             duration = 30,
                             closeButton = TRUE)
          } else {
            showNotification(
              paste("Your booking reference number does not exist, recheck it please"),
              type = "message",
              duration = 30,
              closeButton = TRUE)
          }
          
          my_booked_table <-
            booked_table[booked_table$booker == user_data[['ID']], ]
          
          output$cancel_table <-
            DT::renderDataTable(my_booked_table)
        })
      )
    })
}

