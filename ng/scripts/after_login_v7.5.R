
library(shiny)
library(shinyjs)
library(shinyhelper)
library(shinydashboard)
library(jsonlite)
library(rjson)
library(RJSONIO)
library(DT)
library(DBI)
library(RSQLite)
library(RMySQL)
library(tidyverse)
library(reshape2)


source("../R/update_status.R")
source("../R/CRUD_functions.R")
source("../R/dates_in_next_wk.R")
source("../R/tableShown.R")
source("../R/room_confirm_msg.R")
source("../R/create_candidate_table.R")
source("../R/time_lup.R")
source("../R/helper_fns.R")

# modules
source("../R/searchAvailRoom.R")
source("../R/updateMyRoomStatus.R")
source("../R/cancelBooking.R")
source("../R/help_tab.R")


# database source
options(edwinyu = list(
  "host" = NULL,
  "port" = 0,
  "user" = NULL,
  "password" = NULL
))

drv <<- RSQLite::SQLite()
database <<- "../sql/room_avail.db"

# reset database
# file.copy(from = "schema_room_avail.db", to = database, overwrite = TRUE)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "Room_status_update", updateMyRoomStatusUI("room_status")),
    tabItem(tabName = "Room_booking", searchAvailRoomUI("room_booking")),
    tabItem(tabName = "Room_booked", cancelBookingUI("cancel")),
    tabItem(tabName = "Help", helpUI("help"))
  ),
  
  HTML('<div data-iframe-height></div>')
)

# -------------------------------------------------------------------------

sidebar <- dashboardSidebar(
  collapsed = FALSE,
  sidebarMenu(width = 70,
              menuItem(text = 'Room Status Update',
                       tabName = 'Room_status_update',
                       icon = icon('table')),
              menuItem(text = 'Room booking',
                       tabName = 'Room_booking',
                       icon = icon('table')), 
              menuItem(text = "My Booked Room",
                       tabName = 'Room_booked',
                       icon = icon('table')),
              menuItem(text = "Help",
                       tabName = 'Help',
                       icon = icon('info'))
  )
)

# -------------------------------------------------------------------------

header <-  dashboardHeader(
  title = "Welcome to Polished Fribble (Beta)",
  titleWidth = 450,
  tags$li(class = "dropdown",
          style = "padding: 8px;"),
  tags$li(class = "dropdown",
          tags$a(icon("github"), 
                 title = "See the code on GitHub")))

# -------------------------------------------------------------------------

ui <- dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body,
  skin = "purple"
)

# -------------------------------------------------------------------------

server <- shinyServer(function(input, output, session) {
  
  observe_helpers(help_dir = "helpfiles", withMathJax = FALSE)
  
  indiv_table <- loadData(database, 'individual_information')
  
  credentials <-
    list(auth = credentials_auth,
         info = c(ID = credentials_info_ID))
  
  updateMyRoomStatusServer("room_status", credentials)
  searchAvailRoomServer("room_booking", credentials)
  cancelBookingServer("cancel", credentials)
  helpServer("help", credentials)
})

# -------------------------------------------------------------------------

Sys.setenv(CREDENTIALS_AUTH = TRUE)
# Sys.setenv(SHINYPROXY_USERGROUPS = "shinyroom")

## set user
## COMMENT OUT FOR SHINYSERVER
# Sys.setenv(SHINYPROXY_USERNAME = "Simon")
Sys.setenv(SHINYPROXY_USERNAME = "sruser01")


credentials_info_ID <- Sys.getenv("SHINYPROXY_USERNAME", unset = "")
credentials_auth <- Sys.getenv("CREDENTIALS_AUTH", unset = "")
# shinyproxy_usergroups <- Sys.getenv("SHINYPROXY_USERGROUPS", unset = "")

# run application 
shinyApp(ui = ui, server = server)

