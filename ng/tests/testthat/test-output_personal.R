
source("after_login_v7.5.R")

myApp <- function(...) {
  shinyApp(ui, server, ...)
}


test_that("", {
  
  
})

testServer(myApp(), {
  session$setInputs(date_search = "2020-06-13",
                    login_info,
                    ID,
                    Password)
  
  # print(table_shown)
  print(user_info())
  
  # print(output$personal)
  
})

