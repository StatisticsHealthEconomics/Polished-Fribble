library(shiny)

shinyproxy_username <- Sys.getenv("SHINYPROXY_USERNAME", unset="")
shinyproxy_usergroups <- Sys.getenv("SHINYPROXY_USERGROUPS", unset="")

shinyUI(
    fluidPage(
        titlePanel("Euler's e in arbitrary precision", "Euler's e"),
        tags$br(),

        fluidRow(
            column(2, sliderInput("precision", "Number of Precision Bits", min = 2, max = 256,
                    value = 10))
        ),
        fluidRow(
            column(12, tags$h1(textOutput("result")))

        ),
        tags$br(),
        tags$p("We would like to acknowledge Leonhard Euler for his number and Martin Maechler for his Rmpfr package."),
	tags$hr(),
	tags$strong("SHINYPROXY_USERNAME: "), shinyproxy_username,
	tags$br(),
	tags$br(),
	tags$strong("SHINYPROXY_USERGROUPS: "), shinyproxy_usergroups,
	tags$hr()
    )
)

