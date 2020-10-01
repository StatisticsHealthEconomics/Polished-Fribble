library(shiny)

Sys.setenv(SHINYPROXY_USERNAME="sradmin01")
Sys.setenv(SHINYPROXY_USERGROUPS="shinyroom,shinyroom_adm")

shiny::runApp("pi")
