
# options(shiny.error = browser())

library(shiny)
library(shinyauthr)
library(shinyjs)
library(shinyhelper)
library(shinydashboard)
library(sodium)         # Hashing Passwords with 'sodium'
library(jsonlite)
library(rjson)          # Define json data format for data storage, querying
library(RJSONIO)
library(DT)
library(DBI)
library(RSQLite)
library(RMySQL)
library(tidyverse)
library(reshape2)



source("R/update_status.R")
source("R/CRUD_functions.R")
source("R/dates_in_next_wk.R")
source("R/tableShown.R")
source("R/room_confirm_msg.R")
source("R/create_candidate_table.R")


# database source ----

## free server
# options(edwinyu = list(
#   "host" = "db4free.net",
#   "port" = 3306,
#   "user" = "edwinyu",
#   "password" = "Edwinyrl2019"
# ))
# drv <<- MySQL()
# database <- "room_avail"

# local
options(edwinyu = list(
  "host" = NULL,
  "port" = 0,
  "user" = NULL,
  "password" = NULL
))

drv <<- RSQLite::SQLite()
database <<- "room_avail.db"


is_past <- function(date) as.Date(date) < Sys.Date()

abbr <- function(x){substr(x, 1, 3)}

date_to_weekday <<- function(date) {format(as.Date(date), format = "%a")}

# reset database
# file.copy(from = "schema_room_avail.db", to = database, overwrite = TRUE)


body <- dashboardBody(
  shinyjs::useShinyjs(),
  tags$head(tags$style(".table{margin: 0 auto;}"),
            tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                        type = "text/javascript"),
            includeScript("returnClick.js")
  ),
  shinyauthr::loginUI("login"),
  tabItems(
    tabItem(tabName = "Room_status_update", uiOutput("room_status")),
    tabItem(tabName = "Room_booking", uiOutput("room_booking")),
    tabItem(tabName = "Room_booked", uiOutput("room_booked"))
  ),
  
  # actionButton("change_schedule", "Click Me to Adjust Schedule")
  HTML('<div data-iframe-height></div>')
)

# -------------------------------------------------------------------------

sidebar <- dashboardSidebar(
  collapsed = TRUE,
  sidebarMenu(width = 70,
              menuItem(text = 'Room Status Update',
                       tabName = 'Room_status_update',
                       icon = icon('table')),
              menuItem(text = 'Room booking',
                       tabName = 'Room_booking',
                       icon = icon('table')), 
              menuItem(text = "My Booked Room",
                       tabName = 'Room_booked',
                       icon = icon('table'))
  )
)

# -------------------------------------------------------------------------

header <-  dashboardHeader(
  title = "Welcome to Polished Fribble",
  titleWidth = 450,
  tags$li(class = "dropdown",
          style = "padding: 8px;",
          shinyauthr::logoutUI("logout")),
  tags$li(class = "dropdown",
          tags$a(icon("github"), 
                 href = "https://github.com/paulc91/shinyauthr",
                 title = "See the code on github")))

# -------------------------------------------------------------------------

ui <- dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body,
  skin = "purple"
)

# -------------------------------------------------------------------------

server <- shinyServer(function(input, output, session) {
  
  # Currently read from the database I created remotely (based on a free server)
  # It's safer and maybe run faster if changing for a better server
  # Note that the host, username and password below is for the server
  # NOT FOR THE INDIVIDUAL USERS OF THE LOGIN SYSTEM
  
  observe_helpers(help_dir = "helpfiles", withMathJax = FALSE)
  
  indiv_table <- loadData(database, 'individual_information')
  
  # call the logout module with reactive trigger to hide/show
  logout_init <- callModule(shinyauthr::logout, 
                            id = "logout", 
                            active = reactive(credentials()$user_auth))
  
  # call login module supplying data frame, user and password cols
  # and reactive trigger
  login_info <-
    data.frame(
      ID = indiv_table$UserName,
      Password = indiv_table$Password,
      Password_Hash = sapply(indiv_table$Password, sodium::password_store),
      Permissions = indiv_table$Permissions)
  
  credentials <- callModule(shinyauthr::login,
                            id = "login", 
                            data = login_info,
                            user_col = ID,
                            pwd_col = Password,
                            sodium_hashed = FALSE,
                            log_out = reactive(logout_init()))
  observe({
    if(credentials()$user_auth) {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    } else {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    }
  })
  
  user_info <- reactive({credentials()$info})
  
  # pull out user information from login module
  user_data <- reactive({credentials()$info})
  time <- Sys.time()
  
  next_wk <- dates_in_next_wk() # previously 'a'
  
  
  # diagram displaying current status of personal room information of user  
  output$personal <- DT::renderDataTable({
    req(credentials()$user_auth)
    room_table <- loadData(database, 'new_room_status')
    
    my_room_no <- indiv_table$RoomNumber[indiv_table$UserName == user_data()$ID]  ##TODO: remove duplication of this line
    room_table[room_table$Room_no == my_room_no & 
                 !is_past(room_table$Date), ]},
    options = list(scrollX = TRUE)
  )
  
  # diagram displaying all existing booking of user
  output$cancel <- DT::renderDataTable({
    booked_table <- loadData(database, "room_booked")
    booked_table$day <- date_to_weekday(booked_table$date)
    booked_table[booked_table$booker == user_data()$ID &
                   !is_past(booked_table$date), ]
  })
  
  ## 1. save Updated Room information   
  
  observeEvent(input$save_room, {   
    
    date_update <- input$date_update
    
    lup_time <- c(am = "am", pm = "pm", ampm = "both", "neither")
    
    available_time1 <- paste(input$time1, collapse = "")
    am <- unname(lup_time[available_time1])
    
    room_table <- loadData(database, 'new_room_status')
    
    my_room_no <- indiv_table$RoomNumber[indiv_table$UserName == user_data()$ID]
    
    # If a room has been booked by some else (after user first updating their information)
    # The user cannot change the room status again,
    # and a notification is shown for asking him to contact admin
    
    # examine if the row user wants to update exists in the table
    # if exists, checks if any time has been booked and then decide to update
    # if doesn't exist, row is updated directly without checking
    
    rows_existing <- room_table$Room_no == my_room_no & room_table$Date == date_update
    is_existing_record <-  any(rows_existing)
    
    ##TODO: simplify these ifs...duplication
    
    if (!is_existing_record) {
      
      update_status(use = "write",
                    room_no = my_room_no,
                    date = date_update,
                    am = am,
                    database = database,
                    table = 'new_room_status')
    } else {
      if (any(as.matrix(room_table[rows_existing, ] == "Booked"))) {
        
        showNotification("Someone has booked your room already. Please contact admin.",
                         type = "error",
                         duration = 30,
                         closeButton = TRUE)
      } else {
        update_status(use = "write",
                      room_no = my_room_no,
                      date = date_update,
                      am = am,
                      database = database,
                      table = 'new_room_status')}
    }
    
    output$personal <- DT::renderDataTable(
      {
        req(credentials()$user_auth)
        room_table = loadData(database, 'new_room_status')
        room_table = room_table[room_table$Room_no == my_room_no &
                                  !is_past(room_table$Date), ]
        room_table},
      options = list(scrollX = TRUE)
    )
  })
  
  ## 2. search for available room information
  
  observeEvent(input$search, {
    
    req(input$date_search)
    
    table_shown <- tableShown(input$checkbox_ampm, input$date_search, user_data()$ID)
    
    output$all <- DT::renderDataTable({
      req(credentials()$user_auth)
      table_shown},
      selection = list(mode = "multiple",  # pick date-time cells
                       target = "cell"),
      options = list(scrollX = TRUE))
    
    output$cand_bookings <- renderPrint({
      req(input$all_cells_selected) 
      
      if (nrow(input$all_cells_selected) == 0) {
        cat('Please select the rooms')
      } else { 
        row_id <- input$all_cells_selected[, 1]
        col_id <- input$all_cells_selected[, 2]
        
        booking_candidate <- data.frame(date = table_shown[row_id, "Date"],
                                        day = table_shown[row_id, "Weekday"],
                                        room_no = table_shown[row_id, "Room_no"],
                                        time = colnames(table_shown)[col_id])
        booking_candidate
      }
    })
  })
  
  observeEvent(input$reset, {
    output$all <- NULL
  })  
  
  ## 3. book a room
  
  observeEvent(input$book, {
    
    candidate <- create_candidate_table(input, user_data()$ID)
    
    if (is.null(candidate)) {
      showNotification("Please make a selection",
                       type = "warning",
                       closeButton = TRUE,
                       duration = 30)
      
    } else if (nrow(candidate) == 0) {
      if (nrow(input$all_cells_selected) > 1) {
        showNotification("No room is available for all time slots of your choice.
                         Try selecting fewer time slots at a time.",
                         type = "warning",
                         closeButton = TRUE,
                         duration = 30)
        
      } else if (nrow(input$all_cells_selected) == 1) {
        
        showNotification("No room is available for this time slot so far.",
                         type = "warning",
                         closeButton = TRUE,
                         duration = 30)}
    } else {
      room_no_to_book <- candidate$Room_no
      dates_to_book <- candidate$Date
      
      # change from Available -> Booked
      update_status(use = "booking",
                    room_no = room_no_to_book,
                    date = dates_to_book, 
                    avail = candidate,
                    database = database,
                    table = 'new_room_status')
      
      update_booking(room_no = room_no_to_book,
                     booker = user_data()$ID,
                     date = dates_to_book,
                     time = time_slots,
                     booking_no = NA,
                     database = database,
                     table = "room_booked")
      
      showNotification(room_confirm_msg(num_dates,
                                        room_no_to_book,
                                        dates_to_book,
                                        time_slots),
                       type = "message",
                       duration = 30,
                       closeButton = TRUE)
      
      # update the booking information displayed
      output$cancel <- DT::renderDataTable({
        req(credentials()$user_auth)
        booked_table <- loadData(database, "room_booked")
        booked_table$day <- date_to_weekday(booked_table$date)
        booked_table[booked_table$booker == user_data()$ID & !is_past(booked_table$date), ]
      })
    }
  })
  
  ## 4.cancel the room booking
  observeEvent(input$cancel, {
    
    booked_table <- loadData(database, "room_booked")
    room_table <- loadData(database, 'new_room_status')
    
    date_update <- input$date_update
    
    if (input$booking_no %in% booked_table$booking_no) {
      
      delete_info <- booked_table[booked_table$booking_no == input$booking_no, ]
      
      avail <- room_table %>% 
        filter(Date == delete_info$date,
               Room_no == delete_info$room_no)
      
      avail[1, delete_info$time] <- "Available"
      
      my_room_no <- indiv_table$RoomNumber[indiv_table$UserName == user_data()$ID]
      
      delete_booking(input$booking_no,
                     database = database,
                     table = "room_booked")
      
      update_status(use = "booking",
                    room_no = my_room_no,
                    date = date_update,
                    avail = avail,
                    database = database,
                    table = "new_room_status" )
      
      output$cancel <- DT::renderDataTable({
        req(credentials()$user_auth)
        booked_table <- loadData(database, "room_booked")
        booked_table$day <- date_to_weekday(booked_table$date)
        booked_table[booked_table$booker == user_data()$ID, ]
      })
      
      current <- weekdays(as.POSIXct(Sys.Date()), abbreviate = FALSE)
      
      cancel_confirm_msg <- sprintf("You have successfully canceled the booked room %s \n (%s,%s,",
                                    paste(input$time4, collapse = ' and '), ")",
                                    input$room_book_no2, current, input$day4)
      
      showNotification(cancel_confirm_msg,
                       type = "message",
                       duration = 30,
                       closeButton = TRUE)
    }else{
      showNotification(paste("Your booking reference number does not exist, recheck it please!"),
                       type = "message",
                       duration = 30,
                       closeButton = TRUE)
    }
  })
  
  output$room_status <- renderUI({
    req(credentials()$user_auth)
    fluidPage(box(width = 3,
                  h4("Tell others when your room will be available for booking now!"),
                  wellPanel(
                    dateInput('date_update',
                              label = 'Date',
                              value = Sys.Date()) %>%
                      helper(colour = "mediumpurple1",
                             type = "inline",
                             size = "m",
                             title = "Guidance:",
                             content = c(
                               "- Input the date on which your rooms will be available for booking",
                               "- Then specify the exact period of the day: am, pm, both of them.",
                               "- After saving your chosen time slots, it would be shown on the righthand side.",
                               "- You may always modify your available time slots, so long as it isn't booked by someone else",
                               "- If you input nothing in this table, your room would not appear in any search result (unavailable by default)"))),
                  
                  checkboxGroupInput("time1",
                                     "Time",
                                     choices = c("am","pm"),
                                     selected = ""),
                  actionButton("save_room",
                               "Save",
                               width = "25%")),
              box(width = 9,
                  dataTableOutput(outputId = 'personal')) %>%
                helper(
                  colour = "mediumpurple1",
                  type = "inline",
                  size = "m",
                  content = c("Explaination about cells: ",
                              "  ",
                              "Available  : Your room can be booked for this time slot",
                              "Unavailable: Your room cannot be booked for this time slot",
                              "Booked     : Others have booked your room for this time slot, you can no longer make changes on this day",
                              " ",
                              "To check for the availability of others' rooms and make bookings, please go to Room Booking:) ")
                ),
              tags$head(
                tags$style(
                  HTML(".shiny-notification {position:fixed;top: calc(50%);left: calc(50%);}")
                )
              ))
  })
  
  output$room_booking <- renderUI({
    
    indiv_table <- loadData(database, 'individual_information')
    
    req(credentials()$user_auth)
    fluidRow(box(width = 3,
                 h4("Find out which room can be booked : "),
                 wellPanel(
                   dateInput('date_search',
                             label = 'Date',
                             value = Sys.Date()) %>%
                     helper(colour = "mediumpurple1",
                            type = "inline",
                            size = "m",
                            title = 'Guidance:',
                            content = c("- You can search for others' room availability here, and make your booking in the next section.",
                                        "- Please select the date and the approximate time you wish to use other's office, and then press 'Search'.",
                                        "- If any room satisfys your requirements, it will be shown on the right.")
                     )),
                 checkboxGroupInput("checkbox_ampm",
                                    "Time",
                                    choices = c("am","pm"),
                                    selected = ""),
                 actionButton("search",
                              "Search",
                              width = "25%"),
                 actionButton("reset", "Clear"), 
                 h4("Confirm rooms of your choice:"),
                 verbatimTextOutput('cand_bookings'),
                 verbatimTextOutput('test'),
                 actionButton("book",
                              "Book",
                              width = "25%") %>%
                   helper(
                     colour = "mediumpurple1",
                     type = "inline",
                     size = "m",
                     title = '"How to select the time slot?',
                     content = c(
                       "- Please click on the cells within the data table on the right.",
                       "- Corresponding room information would appear in the box below.",
                       "- You may then confirm your booking, but notice that 'Booked' or 'Unavailable' rooms cannot be booked"
                     )
                   ),
                 tags$head(
                   tags$style(
                     HTML(".shiny-notification {position:fixed;top: calc(50%);left: calc(50%);}"
                     )
                   )
                 )
    ),
    box(width = 9,
        dataTableOutput(outputId = 'all'))
    )
  })
  
  output$room_booked <- renderUI({
    req(credentials()$user_auth)
    fluidPage(box(width = 3,
                  h4("Cancel the booked room"),
                  textInput("booking_no","Input booking number here:", value = "") %>%
                    helper(
                      colour = "mediumpurple1",
                      type = "inline",
                      size = "m",
                      title = 'Guidance:',
                      content = c("Inseroom_table the booking number to the room that you no longer need")),
                  actionButton("cancel",
                               "Cancel Booking",
                               width = "50%"),
                  tags$head(
                    tags$style(
                      HTML(".shiny-notification {position:fixed;top: calc(50%);left: calc(50%);}"
                      )
                    )
                  )
    ),
    box(width = 9,
        dataTableOutput(outputId = 'cancel'))
    )
  })
})

# Run the application 
shinyApp(ui = ui, server = server)
# runApp("after_login_v7.5.R", display.mode = "showcase")

