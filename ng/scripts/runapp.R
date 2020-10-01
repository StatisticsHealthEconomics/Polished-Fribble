library(shiny)

Sys.setenv(CREDENTIALS_INFO_ID = "Simon")
Sys.setenv(CREDENTIALS_AUTH = TRUE)

shiny::runApp("after_login_v7.5.R")