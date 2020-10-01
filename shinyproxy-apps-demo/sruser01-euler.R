library(shiny)

Sys.setenv(SHINYPROXY_USERNAME = "sruser01")
Sys.setenv(SHINYPROXY_USERGROUPS = "shinyroom")

shiny::runApp("euler")
