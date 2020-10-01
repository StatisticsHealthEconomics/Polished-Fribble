# devtools::install_github("paulc91/shinyauthr")

library(shiny)
library(shinyauthr)
library(shinyjs)
# Querying dataset
library(tidyverse)
# Hashing Passwords with 'sodium'
library(sodium)
library(shinydashboard)
library(jsonlite)
# Define a json data format for data storage and querying
library(rjson)
library(RSQLite)
library(RJSONIO)
library(RSQLite)
library(DBI)
library(RMySQL)
library(DT)
library(shinyhelper)
library(shinyWidgets)


#### body #####
body <- dashboardBody(
  shinyjs::useShinyjs(),
  tags$head(tags$style(".table{margin: 0 auto;}"),
            tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                        type="text/javascript"),
            includeScript("returnClick.js")
  ),
  shinyauthr::loginUI("login"),
  tabItems(
    tabItem("Room_Status_Update", uiOutput("UI1")),
    tabItem("Room_booking", uiOutput("UI2")),
    tabItem("Room_booked", uiOutput("UI3"))
  ),
  
  # actionButton("change_schedule", "Click Me to Adjust Schedule")
  HTML('<div data-iframe-height></div>')
  
)# End of body


#### Side bar #####
sidebar<-dashboardSidebar(
  collapsed = TRUE,
  sidebarMenuOutput('menu')
)



#### Define UI for application that draws a histogram #####
ui <- dashboardPage(
  dashboardHeader(
    title = "Welcome to Polished Fribble!",
    titleWidth = 450,
    tags$li(class = "dropdown", style = "padding: 8px;",
            shinyauthr::logoutUI("logout")),
    tags$li(class = "dropdown", tags$a(icon("github"), 
                                       href = "https://github.com/paulc91/shinyauthr",
                                       title = "See the code on github"))),
  sidebar = sidebar,
  body = body,
  skin = "purple"
)











# Define server logic required to draw a histogram ####
server <- shinyServer(function(input,output,session){
  
  
  ############################# Database setting ############################
  library(RMySQL)
  
  #Currently read from the database I created remotely(based on a free server)
  #It's safer and maybe run faster if changing for a better server
  #Note that the host,username and password below is for the server
  #NOT FOR THE INDIVIDUAL USERS OF THE LOGIN SYSTEM
  
  options(edwinyu = list(
    "host" = "db4free.net",
    "port" = 3306,
    "user" = "edwinyu",
    "password" = "Edwinyrl2019"
  ))
  
  observe_helpers(help_dir = "helpfiles", withMathJax = FALSE)
  
  
  ## DR_Expand function: expands the date range
  ## e.g. "2020-01-28" - "2020-01-30" will be expanded to be e.g. c("2020-01-28","2020-01-29","2020-01-30")
  DR_Expand <- function(dateRange){
    Date_start <- dateRange[1]
    Date_end <- dateRange[2]
    lengthout <- as.numeric(strsplit(as.character(as.Date(Date_end) - as.Date(Date_start))," ")[[1]])
    
    as.character(seq(as.Date(Date_start), by = "day", length.out = lengthout+1))
  }
  
  
  ## After_Change function: changes the room status
  ## e.g. "Not in office" will be changed to "In Office"
  After_Change <-  function(status){
    if(status == "Not in office"){
      return("In office")
    }else if(status=="In Office"){
      return(("Not in office"))
    }else{
      showNotification("This time slot has been booked, please contact the administrator for further changes!")
      }
  }
  
  
  #load the table from the given database   
  loadData <- function(database,table) {
    db <- dbConnect(MySQL(), dbname = database, host = options()$edwinyu$host, 
                    port = options()$edwinyu$port, user = options()$edwinyu$user, 
                    password = options()$edwinyu$password)
    query <- sprintf("SELECT * FROM %s", table)
    data <- dbGetQuery(db, query)
    dbDisconnect(db)
    data
  }
  #For updating rows in table "new_room_status" from db "room_avail"
  ##when use="write", the function inputs am="am"/"pm"/"both"/"neither", used for writing personal room status to the table
  #when use="booking", the function inputs avail
  #avail is an n by 8 matrix, each row corresponds to 8 periods for certain room on a specific day
  
  #new_room_status is a table I created in MySQL with Room_no and Date as PRIMARY,
  #so that when updating rows with the same Room_no and Date existing in the table
  #it modifies the exisitng row rather than adding a new row   
  update_status <- function(room_no,date,use="write",am=NA,avail=NA,database,table) {
    date_to_weekday<-function(date){format(as.Date(date), format="%a")}
    if(use=="write"){
      A=data_frame(room_no=room_no,date=date,am=am,avail=NA)
      
      A[A$am=="am",4]=paste0(paste(rep("Available",3),collapse = "','"),"','",paste(rep("Unavailable",5),collapse = "','"))
      A[A$am=="pm",4]=paste0(paste(rep("Unavailable",3),collapse = "','"),"','",paste(rep("Available",5),collapse = "','"))
      A[A$am=="both",4]=paste0(paste(rep("Available",3),collapse = "','"),"','",paste(rep("Available",5),collapse = "','"))
      A[A$am=="neither",4]=paste0(paste(rep("Unavailable",3),collapse = "','"),"','",paste(rep("Unavailable",5),collapse = "','"))
      A['weekday']=date_to_weekday(A$date)
      
      AM=A[A$am=="am",]
      PM=A[A$am=="pm",]
      Both=A[A$am=="both",]
      Neither=A[A$am=="neither",]
      
      if("am" %in% am){
        query_am= paste0(paste0("('",paste(AM$date,AM$weekday,AM$room_no,AM$avail,sep="','"),"')"),collapse = ",") 
      }
      else{ query_am=NULL    }
      if("pm" %in% am){query_pm=paste0(paste0("('",paste(PM$date,PM$weekday,PM$room_no,PM$avail,sep="','"),"')"),collapse = ",") } else{query_pm=NULL}
      if("both" %in% am){query_both=paste0(paste0("('",paste(Both$date,Both$weekday,Both$room_no,Both$avail,sep="','"),"')"),collapse = ",") } else{query_both=NULL}
      if("neither" %in% am){query_neither=paste0(paste0("('",paste(Neither$date,Neither$weekday,Neither$room_no,Neither$avail,sep="','"),"')"),collapse = ",") } else{query_neither=NULL}
      
      l=c(query_am,query_pm,query_both,query_neither)
      
      q=paste(l,collapse=",")
      
      
    }else if(use=="booking"){
      availability=paste(avail[,1],avail[,2],avail[,3],avail[,4],avail[,5],avail[,6],avail[,7],avail[,8],sep="','")
      A=data_frame(room_no=room_no,date=date,avail=availability)
      A['weekday']=date_to_weekday(A$date)
      q=paste0(paste0("('",paste(A$date,A$weekday,A$room_no,A$avail,sep="','"),"')"),collapse = ",") 
      
    }
    
    
    query=paste0("INSERT INTO ",table," VALUES ",q," ON DUPLICATE KEY UPDATE Date=VALUES(Date),Weekday=VALUES(Weekday),Room_no=VALUES(Room_no),
9am_10am=VALUES(9am_10am),10am_11am=VALUES(10am_11am),11am_12pm=VALUES(11am_12pm),12pm_1pm=VALUES(12pm_1pm),1pm_2pm=VALUES(1pm_2pm),
2pm_3pm=VALUES(2pm_3pm),3pm_4pm=VALUES(3pm_4pm),4pm_5pm=VALUES(4pm_5pm)")
    print(query)#for debug
    db <- dbConnect(MySQL(), dbname = database, host = options()$edwinyu$host, 
                    port = options()$edwinyu$port, user = options()$edwinyu$user, 
                    password = options()$edwinyu$password)
    dbGetQuery(db, query)
    dbDisconnect(db)
    
    
    
  }
  
  #For deleting rows in table "new_room_status" from db "room_avail"    
  delete <- function(room_no,date_1,date_2=NULL,database,table) {
    db <- dbConnect(MySQL(), dbname = database, host = options()$edwinyu$host, 
                    port = options()$edwinyu$port, user = options()$edwinyu$user, 
                    password = options()$edwinyu$password)
    if (is.null(date_2)){
      query <- paste0('DELETE FROM ',table," WHERE Room_no = '", room_no,"' AND ",
                      "Date = '", date_1,"'")
    }else{
      query <- paste0('DELETE FROM ',table," WHERE Room_no = '", room_no,"' AND ",
                      "Date BETWEEN '", date_1,"' AND '",date_2,"'")
    }
    
    print(query)#for debug
    dbSendQuery(db, query)
    dbDisconnect(db)
  }
  
  
  #functions for updating booking(for table room_booked)
  #room_booked is a table I created in MySQL with PRIMARY booking_no   
  update_booking <- function(room_no,booker,date,time,booking_no,database,table){
    db <- dbConnect(MySQL(), dbname = database, host = options()$edwinyu$host, 
                    port = options()$edwinyu$port, user = options()$edwinyu$user, 
                    password = options()$edwinyu$password)
    query=paste0("INSERT INTO ",table," VALUES ('",booking_no,"','",date,"','",time,"','",room_no,"','",booker,"') ON DUPLICATE KEY UPDATE room_no=VALUES(room_no),booker=VALUES(booker),date=VALUES(date),time=VALUES(time)")
    print(query)
    dbGetQuery(db, query)
    dbDisconnect(db)
    
  }
  #function for deleting booking from room_booked  
  delete_booking <- function(booking_no,database,table) {
    db <- dbConnect(MySQL(), dbname = database, host = options()$edwinyu$host, 
                    port = options()$edwinyu$port, user = options()$edwinyu$user, 
                    password = options()$edwinyu$password)
    
    query <- paste0('DELETE FROM ',table," WHERE booking_no = '",booking_no,"'")
    
    
    print(query)#for debug
    dbSendQuery(db, query)
    dbDisconnect(db)
  }
  
  
  
  individual <- loadData('room_avail','individual_information')
  individual_rb <- loadData('room_avail','new_room_status')
  
  
  
  
  ################### call the logout module with reactive trigger to hide/show ####
 logout_init <- callModule(shinyauthr::logout, 
                            id = "logout", 
                            active = reactive(credentials()$user_auth))
  
  # call login module supplying data frame, user and password cols
  # and reactive trigger
  login_info <- data.frame(ID=individual$UserName,Password=individual$Password,Password_Hash = sapply(individual$Password, sodium::password_store),Permissions=individual$Permissions)
  
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
  
  
  
  
  
  
  
  ################## pulls out the user information returned from login module ####
  user_data <- reactive({credentials()$info})
  time <- Sys.time()
  
  # a is a dataframe consisting two columns， date and day
  # which corresponds to the date and day of the very next week    
  a <- data.frame(date=rep(NA,5),day = c("Mon","Tue","Wed","Thu", "Fri"))
  date_to_weekday<-function(date){format(as.Date(date), format="%a")}
  
  if(date_to_weekday(Sys.Date()) %in% c("Sun","Sat") ){
    d=Sys.Date()
    while( date_to_weekday(d) %in% c("Sun","Sat") ){
      d=d+1
    }
    while( !(date_to_weekday(d) %in% c("Sun","Sat")) ){
      a[a$day==date_to_weekday(d),]$date = as.character(d)
      d=d+1
    }
    
  }else{
    
    d=Sys.Date()+7
    while( !(date_to_weekday(d) %in% c("Sun","Sat")) ){
      d=d-1
    }
    d=d+1
    while( !(date_to_weekday(d) %in% c("Sun","Sat")) ){
      a[a$day==date_to_weekday(d),]$date = as.character(d)
      d=d+1
    }
    
  }   

  
  
  
  
  ##################
  #     server     #
  ##################
  
  
  ## 1. Save Updated Room information #####
  observeEvent(input$search2, 
               { 
                 if(as.integer(input$dateRange[2]-input$dateRange[1]) >= 0){
                   
                 updated_room=loadData('room_avail','new_room_status')
                 updated_room = updated_room[updated_room$Room_no==individual$RoomNumber[individual$UserName==user_data()$ID] & updated_room$Date%in%DR_Expand(input$dateRange),]
                 
                 table_shown <- data.frame("Date" = DR_Expand(input$dateRange),
                                           "Weekday" = date_to_weekday(DR_Expand(input$dateRange)),
                                           "Room_no" = rep(individual$RoomNumber[user_data()$ID==individual$UserName],length(DR_Expand(input$dateRange))),
                                           "AM" = rep("In Office",length(DR_Expand(input$dateRange))), "PM"= rep("In Office",length(DR_Expand(input$dateRange))),stringsAsFactors = FALSE)
                 
                 AM <- vector()
                 PM <- vector()
                 
                 for(i in 1:nrow(updated_room[,4:6])){
                   AM[i] <- if("Unavailable"%in%updated_room[i,4:6]){
                     "In Office"
                   }else if("Booked"%in%updated_room[i,4:6]){
                     "Booked"
                   }else{
                     "Not in office"
                   }
                   PM[i] <- if("Unavailable"%in%updated_room[i,7:11]){
                     "In Office"
                   }else if("Booked"%in%updated_room[i,7:11]){
                     "Booked"
                   }else{
                     "Not in office"
                   }
                   
                 }
                 
                 AMPM <- data.frame("AM" = AM,"PM" = PM,stringsAsFactors=FALSE )
                 Table_shown <- cbind(updated_room[,1:3],AMPM)
                 
                 table_shown[table_shown$Date%in%Table_shown$Date,] <- Table_shown
                 
                 output$personal1 <- DT::renderDataTable({
                   req(credentials()$user_auth)
                   table_shown
                   
                 },selection=list(mode="multiple", target="cell"),
                   options = list(scrollX = TRUE)
                 )
                 
                 output$slot_updatings <- renderPrint({ 
                   
                   req(input$personal1_cells_selected)
                   if(nrow(input$personal1_cells_selected)==0){
                     cat('Please select the rooms')
                   }else{ 
                     row_id=input$personal1_cells_selected[,1]
                     col_id=input$personal1_cells_selected[,2]
                     cell = input$personal1_cells_selected
                     after_change = vector()
                     for (i in 1:length(table_shown[cell])) {
                       after_change[i] <- After_Change(table_shown[cell][i])
                     }
                     slot_update = data.frame(date=table_shown[row_id,1],day=table_shown[row_id,2],room_no=table_shown[row_id,3],time=colnames(table_shown)[col_id],status = table_shown[cell], after_change = after_change)
                     slot_update
                     
                   }
 
                 })
                
                 }else{
                  showNotification("recheck with your input date!")
                   }
               })
  
  ## update selected time slot information
  observeEvent(input$change,
               {
                 updated_room=loadData('room_avail','new_room_status')
                 updated_room = updated_room[updated_room$Room_no==individual$RoomNumber[individual$UserName==user_data()$ID] & updated_room$Date%in%DR_Expand(input$dateRange),]
                 
                 table_shown <- data.frame("Date" = DR_Expand(input$dateRange),
                                           "Weekday" = date_to_weekday(DR_Expand(input$dateRange)),
                                           "Room_no" = rep(individual$RoomNumber[user_data()$ID==individual$UserName],length(DR_Expand(input$dateRange))),
                                           "AM" = rep("In Office",length(DR_Expand(input$dateRange))), "PM"= rep("In Office",length(DR_Expand(input$dateRange))),stringsAsFactors = FALSE)
                 
                 AM <- vector()
                 PM <- vector()
                 
                 for(i in 1:nrow(updated_room[,4:6])){
                   AM[i] <- if("Unavailable"%in%updated_room[i,4:6]){
                     "In Office"
                   }else if("Booked"%in%updated_room[i,4:6]){
                     "Booked"
                   }else{
                     "Not in office"
                   }
                   PM[i] <- if("Unavailable"%in%updated_room[i,7:11]){
                     "In Office"
                   }else if("Booked"%in%updated_room[i,7:11]){
                     "Booked"
                   }else{
                     "Not in office"
                   }
                   
                 }
                 
                 AMPM <- data.frame("AM" = AM,"PM" = PM,stringsAsFactors = FALSE)
                 Table_shown <- cbind(updated_room[,1:3],AMPM)
                 
                 table_shown[table_shown$Date%in%Table_shown$Date,] <- Table_shown
                 
                 req(input$personal1_cells_selected)
                 if(nrow(input$personal1_cells_selected)==0){
                   cat('Please select the rooms')
                 }else{ 
                   row_id=input$personal1_cells_selected[,1]
                   col_id=input$personal1_cells_selected[,2]
                   cell = input$personal1_cells_selected
                   for (i in 1:length(row_id)){
                     
                     if(col_id[i] == 4){
                       
                       if(table_shown[cell][i] == "In Office" && table_shown[row_id[i],5] == "In Office"){
                         
                         update_status(use="write",room_no = table_shown[row_id,3],
                                       date = as.Date(table_shown[row_id,1]),
                                       am = "am",database='room_avail',table = 'new_room_status')
                         
                       }else if(table_shown[cell][i] == "In Office" && table_shown[row_id[i],5] == "Not in office"){
                        
                          update_status(use="write",room_no = table_shown[row_id,3],
                                       date = as.Date(table_shown[row_id,1]),
                                       am = "both",database='room_avail',table = 'new_room_status')
                         
                       }else if(table_shown[cell][i] == "Not in office" && table_shown[row_id[i],5] == "Not in office"){
                         
                         update_status(use="write",room_no = table_shown[row_id,3],
                                       date = as.Date(table_shown[row_id,1]),
                                       am = "pm",database='room_avail',table = 'new_room_status')
                         
                       }else if(table_shown[cell][i] == "Not in office" && table_shown[row_id[i],5] == "In Office"){
                         update_status(use="write",room_no = table_shown[row_id,3],
                                       date = as.Date(table_shown[row_id,1]),
                                       am = "neither",database='room_avail',table = 'new_room_status')
                         
                       }
                       
                   }else if(col_id[i]==5){
                     
                     if(table_shown[cell][i] == "In Office" && table_shown[row_id[i],4] == "In Office"){
                       update_status(use="write",room_no = table_shown[row_id,3],
                                     date = as.Date(table_shown[row_id,1]),
                                     am = "pm",database='room_avail',table = 'new_room_status')
                     }else if(table_shown[cell][i] == "In Office" && table_shown[row_id[i],4] == "Not in office"){
                       
                       update_status(use="write",room_no = table_shown[row_id,3],
                                     date = as.Date(table_shown[row_id,1]),
                                     am = "both",database='room_avail',table = 'new_room_status')
                       
                     }else if(table_shown[cell][i] == "Not in office" && table_shown[row_id[i],4] == "Not in office"){
                       
                       update_status(use="write",room_no = table_shown[row_id,3],
                                     date = as.Date(table_shown[row_id,1]),
                                     am = "am",database='room_avail',table = 'new_room_status')
                       
                     }else if(table_shown[cell][i] == "Not in office" && table_shown[row_id[i],4] == "In Office"){
                       update_status(use="write",room_no = table_shown[row_id,3],
                                     date = as.Date(table_shown[row_id,1]),
                                     am = "neither",database='room_avail',table = 'new_room_status')
                       
                     }
                  
                     }
                     
                     
                     
                   }
                   
                   
                 }
                 
                 ## print out the new table_shown
                 updated_room=loadData('room_avail','new_room_status')
                 updated_room = updated_room[updated_room$Room_no==individual$RoomNumber[individual$UserName==user_data()$ID] & updated_room$Date%in%DR_Expand(input$dateRange),]
                 
                 table_shown <- data.frame("Date" = DR_Expand(input$dateRange),
                                           "Weekday" = date_to_weekday(DR_Expand(input$dateRange)),
                                           "Room_no" = rep(individual$RoomNumber[user_data()$ID==individual$UserName],length(DR_Expand(input$dateRange))),
                                           "AM" = rep("In Office",length(DR_Expand(input$dateRange))), "PM"= rep("In Office",length(DR_Expand(input$dateRange))),stringsAsFactors = FALSE)
                 
                 AM <- vector()
                 PM <- vector()
                 
                 for(i in 1:nrow(updated_room[,4:6])){
                   AM[i] <- if("Unavailable"%in%updated_room[i,4:6]){
                     "In Office"
                   }else if("Booked"%in%updated_room[i,4:6]){
                     "Booked"
                   }else{
                     "Not in office"
                   }
                   PM[i] <- if("Unavailable"%in%updated_room[i,7:11]){
                     "In Office"
                   }else if("Booked"%in%updated_room[i,7:11]){
                     "Booked"
                   }else{
                     "Not in office"
                   }
                   
                 }
                 
                 AMPM <- data.frame("AM" = AM,"PM" = PM,stringsAsFactors = FALSE)
                 Table_shown <- cbind(updated_room[,1:3],AMPM)
                 
                 table_shown[table_shown$Date%in%Table_shown$Date,] <- Table_shown
                 
                 output$personal1 <- DT::renderDataTable({
                   req(credentials()$user_auth)
                   table_shown
                 },selection=list(mode="multiple", target="cell"),
                 options = list(scrollX = TRUE)
                 )
                 
                 showNotification("Your room status has been changed!")
                 

               })
  
  
  ## 2. search for available room information #####
  observeEvent(input$search,
               { 
                 req(input$date2)
                 room_table = loadData('room_avail','new_room_status')
                 abbr=function(x){substr(x, 1, 3)}
                 avail_am=c()
                 avail_pm=c()
                 avail_both=c()
                 for(i in 1:nrow(room_table)){
                   avail_am[i]=any(room_table[i,2:4]=="Available")
                   avail_pm[i]=any(room_table[i,5:9]=="Available")
                   avail_both[i]=any(room_table[i,2:9]=="Available")
                 }#shiny-tab-Room_booking
                 
                 search_time=paste(input$time2,collapse ="")
                 
                 if(search_time=="ampm"){
                   avail=avail_both
                 }else if(search_time=="am"){
                   avail=avail_am
                 }else if(search_time=="pm"){
                   avail=avail_pm
                 }else{
                   avail=rep(TRUE,nrow(room_table))
                 }
                 
                 
                 
                 selected_rt=room_table[avail & room_table$Date %in% as.character(input$date2), ]
                 table_shown=selected_rt[selected_rt$Room_no!=individual$RoomNumber[individual$UserName==user_data()$ID],]
                 
                 
                 output$all <- DT::renderDataTable({
                   req(credentials()$user_auth)
                   table_shown
                 },selection=list(mode="multiple", target="cell"),
                 options = list(scrollX = TRUE))
                 
                 
                 
                 output$cand_bookings <- renderPrint({ 
                   
                   req(input$all_cells_selected)
                   if(nrow(input$all_cells_selected)==0){
                     
                     cat('Please select the rooms')
                     
                   }else{ 
                     
                     row_id=input$all_cells_selected[,1]
                     col_id=input$all_cells_selected[,2]
                     booking_candidate=data.frame(date=table_shown[row_id,1],day=table_shown[row_id,2],room_no=table_shown[row_id,3],time=colnames(table_shown)[col_id])
                     booking_candidate
                     
                   }
                   
                   
                 })
                 
                 
                 
                 
                 
               })
                 
  ## 3. book the room ####
  observeEvent(input$book,{rt=loadData('room_avail','new_room_status')
  
  room_table=loadData('room_avail','new_room_status')
  abbr=function(x){substr(x, 1, 3)}
  avail_am=c()
  avail_pm=c()
  avail_both=c()
  for(i in 1:nrow(room_table)){
    avail_am[i]=any(room_table[i,2:4]=="Available")
    avail_pm[i]=any(room_table[i,5:9]=="Available")
    avail_both[i]=any(room_table[i,2:9]=="Available")
  }#shiny-tab-Room_booking
  
  search_time=paste(input$time2,collapse ="")
  
  if(search_time=="ampm"){
    avail=avail_both
  }else if(search_time=="am"){
    avail=avail_am
  }else if(search_time=="pm"){
    avail=avail_pm
  }else{
    avail=rep(TRUE,nrow(room_table))
  }
  
  
  
  selected_rt=room_table[avail & room_table$Date %in% as.character(input$date2), ]
  table_shown=selected_rt[selected_rt$Room_no!=individual$RoomNumber[individual$UserName==user_data()$ID],]
  
  
  index=input$all_cells_selected
  row_id=input$all_cells_selected[,1]
  col_id=input$all_cells_selected[,2]
  booking_candidate=data.frame(date=table_shown[row_id,1],day=table_shown[row_id,2],room_no=table_shown[row_id,3],time=colnames(table_shown)[col_id])
  
  
  unique_row_no=as.vector(unique(input$all_cells_selected[,1]))
  booking_candidate2=matrix(data=list(),ncol=4,nrow=length(unique_row_no))
  
  for(j in 1:length(unique_row_no)){
    booking_candidate2[[j,1]]=table_shown[unique_row_no[j],1]  
    booking_candidate2[[j,2]]=table_shown[unique_row_no[j],2] 
    booking_candidate2[[j,3]]=table_shown[unique_row_no[j],3] 
    booking_candidate2[[j,4]]=as.vector(index[index[,1]==unique_row_no[j],2])
    
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  intervals= as.matrix(booking_candidate2[,4])
  
  interval_avail=NULL
  for(i in 1:length(unique_row_no)){ 
    interval_avail=rbind(interval_avail,apply( as.matrix(rt[,matrix(booking_candidate2[,4][[i]])]=="Available"), 1, all))
  }
  
  
  
  
  #The mechanism of booking supports only booking one room at a time
  #User chooses day and (multiple) time slots to make the booking
  #If no single room satisfying all requirements, a notification is shown to let users select fewer time slots at a time
  candidate=NULL
  for(i in 1:length(unique_row_no)){ 
    candidate=rbind(candidate,rt[ interval_avail[i,] & rt$Date==as.character(booking_candidate2[i,1]) & rt$Room_no!=individual$RoomNumber[individual$UserName==user_data()$ID] & rt$Room_no == booking_candidate2[i,3],])
  }
  
  
  
  
  if(nrow(candidate)==0){
    if(length(intervals)>1){
      showNotification("No room is available for all time slots of your choice. Try selecting fewer time slots at a time.",type="warning",closeButton = TRUE,duration=30)
      
    }else if(length(intervals)==1){
      
      showNotification("No room is available for this time slot so far.",type="warning",closeButton = TRUE,duration=30)
      
    }
    
  }else{
    
    
    room_no=candidate$Room_no
    room_status=candidate[1:nrow(intervals),4:11]
    
    
    for(i in 1:nrow(intervals)){
      room_status[i,][intervals[[i]]-3]="Booked"
    }
    
    update_status(use="booking",room_no = room_no,
                  date = as.character(booking_candidate2[,1]), 
                  avail=room_status,
                  database='room_avail',table = 'new_room_status')
    room_confirm=''
    for(i in 1:length(unique_row_no)){
      room_confirm=paste(room_confirm,'room',room_no[i],"\n","(",booking_candidate2[i,1],",",date_to_weekday(as.character(booking_candidate2[i,1])),",",paste(colnames(rt)[intervals[[i]]],collapse=' and '),")",sep=" ")
      
    }
    
    room_confirm=paste("You have successfully booked",room_confirm)
    
    showNotification(room_confirm,type="message",duration=30,closeButton = TRUE)
    
    # update_booking(room_no=booking_candidate[,3],booker=user_data()$ID,date = as.character(booking_candidate[,1]),time=booking_candidate[,4],booking_no = as.integer(Sys.time()),"room_avail","room_booked")
    
    #update the booking information displayed
    output$cancel <- DT::renderDataTable({
      req(credentials()$user_auth)
      room_booked=loadData("room_avail","room_booked")
      room_booked$day <- date_to_weekday(room_booked$date)
      room_booked[room_booked$booker==user_data()$ID & as.Date(room_booked$date)>=Sys.Date(),]
      
    })
  }
  
  }
  
  )
  
  
  
  ## 4.cancel the room booking ####
  observeEvent(input$cancel,
               {
                 room_booked <- loadData("room_avail","room_booked")
                 rt <- loadData('room_avail','new_room_status')
                 
                 date_update = input$date1
                 
                 if(input$booking_no %in% room_booked$booking_no){
                   delete_Info <- room_booked[room_booked$booking_no==input$booking_no,]
                   
                   delete_booking(input$booking_no,"room_avail","room_booked")
                   avail <- rt[rt$Date==delete_Info$date,]
                   avail <- avail[avail$Room_no==delete_Info$room_no,]
                   avail[1,delete_Info$time] <- "Available"
                   update_status(use = "booking",room_no = individual$RoomNumber[individual$UserName==user_data()$ID], date = date_update,avail = avail,database = "room_avail",table = "new_room_status" )
                   
                   output$cancel <- DT::renderDataTable({
                     req(credentials()$user_auth)
                     room_booked=loadData("room_avail","room_booked")
                     room_booked$day <- date_to_weekday(room_booked$date)
                     room_booked[room_booked$booker==user_data()$ID,]
                     
                   })
                   
                   current <- weekdays(as.POSIXct(Sys.Date()), abbreviate = F)
                   cancel_confirm=paste("You have successfully canceled the booked room",input$room_book_no2,"\n","(",current,",",input$day4,",",paste(input$time4,collapse=' and '),")",sep="")
                   showNotification(cancel_confirm,type="message",duration=30,closeButton = TRUE)
                 }else{
                   booking_no_error <- paste("Your booking reference number does not exist, recheck it please!")
                   showNotification(booking_no_error,type="message",duration=30,closeButton = TRUE)
                 }
                 
               })

  
  
  
  
  ##################
  #       UI       #
  ##################
  
  #### UI1 - Room status update ####
  output$UI1 <- renderUI({
    req(credentials()$user_auth)
    fluidPage(box(width=4,
                  h4("Tell others when your room will be available for booking now!"),
                  wellPanel(
                    dateRangeInput('dateRange',
                                   label = 'Date range input: yyyy-mm-dd',
                                   start = Sys.Date(), end = Sys.Date()+2
                    )%>%
                      helper(
                        colour = "mediumpurple1",
                        type = "inline",
                        sizeb  = "m",
                        title = "Guidance:",
                        content = c(
                          "- Input the date on which your rooms will be available for booking",
                          "- Then specify the exact period of the day: am, pm, both of them.",
                          "- After saving your chosen time slots, it would be shown on the righthand side.",
                          "- You may always modify your available time slots, so long as it isn't booked by someone else",
                          "- If you input nothing in this table, your room would not appear in any search result (unavailable by default)"))),
                  
                  
                  
                  actionButton("search2","Search",width = "25%"),
                  
                  h4("Confirm your updating information:"),
                  verbatimTextOutput('slot_updatings'),
                  verbatimTextOutput("test"),
                  actionButton("change","Change Status",width = "40%")
                  
    ),
    box(width = 8,
        dataTableOutput(outputId = 'personal1'))%>%
      helper(
        colour = "mediumpurple1",
        type = "inline",
        size = "m",
        content = c("Explaination about cells: ","  ",
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
    )
    )
  })
  
  
  #### UI2 - Room booking ####
  output$UI2 <- renderUI({
    req(credentials()$user_auth)
    fluidRow(box(width=3,
                 h4("Find out which room can be booked : "),
                 wellPanel(
                   dateInput('date2',
                             label = 'Date',
                             value = Sys.Date()
                             
                   )%>%helper(
                     colour = "mediumpurple1",
                     type = "inline",
                     size = "m",
                     title='Guidance:',
                     content = c("- You can search for others' room availability here, and make your booking in the next section.",
                                 "- Please select the date and the approximate time you wish to use other's office, and then press 'Search'.",
                                 "- If any room satisfys your requirements, it will be shown on the right.")
                     
                   )),
                 checkboxGroupInput("time2","Time",choices = c("am","pm"),selected = ""),
                 actionButton("search","Search",width = "25%"),
                 
                 h4("Confirm rooms of your choice:"),
                 verbatimTextOutput('cand_bookings'),verbatimTextOutput('test'),
                 
                 
                 actionButton("book","Book",width = "25%")%>%
                   helper(
                     colour = "mediumpurple1",
                     type = "inline",
                     size = "m",title='"How to select the time slot?',
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
  
  #### UI3 - My booked room ####
  output$UI3 <- renderUI({
    req(credentials()$user_auth)
    fluidPage(box(width = 3,
                  h4("Cancel the booked room"),
                  textInput("booking_no","Input booking number here:",value = "")%>%
                    helper(
                      colour = "mediumpurple1",
                      type = "inline",
                      size = "m",title='Guidance:',
                      content =c("Insert the booking number to the room that you no longer need")),
                  actionButton("cancel","Cancel Booking",width = "50%"),
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
  
  
  output$menu=renderMenu({
    req(credentials()$user_auth)
    sidebarMenu(width=70,
                menuItem(text='Room Status Update', tabName='Room_Status_Update',icon=icon('table')),
                menuItem(text='Room booking', tabName='Room_booking',icon=icon('table')), 
                menuItem(text= "My Booked Room", tabName='Room_booked',icon=icon('table')))
    
  })
  
  
  
}) 

# Run the application 
shinyApp(ui = ui, server = server)





