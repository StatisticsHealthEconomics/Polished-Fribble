
#
helpUI <- function(id) {
  
  ns <- NS(id)
  uiOutput(ns("help"))
}


#
helpServer <- function(id, credentials) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      output$help <- renderUI({
        
        ns <- session$ns
        
        # req(credentials$user_auth)
        
        tagList(
          h1("What is Polished Fribble?"),
          p("p creates a paragraph of text."),
          p("A new p() command starts a new paragraph.
          Supply a style attribute to change the format of the entire paragraph.",
            style = "font-family: 'times'; font-si16pt"),
          strong("strong() makes bold text."),
          em("em() creates italicized (i.e, emphasized) text."),
          br(),
          h1("How to use"),
          h2("Changing your personal room status"),
          h2("Booking a room"),
          h2("Cancelling your room booking")
        )
      }) 
    }
    
  )
}