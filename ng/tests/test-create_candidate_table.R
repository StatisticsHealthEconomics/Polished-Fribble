
database <<- "room_avail.db"

input <- list(all_cells_selected = matrix(c(1,4), nrow = 1), # 9am_10am
              checkbox_ampm = "ampm",
              date_search = "2020-07-15")

room_table <- loadData(database, 'new_room_status')

create_candidate_table(input,
                       user_ID = "Simon")

input$all_cells_selected <- matrix(c(1,11), nrow = 1) # 4pm-5pm
create_candidate_table(input,
                       user_ID = "Simon")


input$date_search <-  "2020-07-14"
input$all_cells_selected <- matrix(c(1,4), nrow = 1)
create_candidate_table(input,
                       user_ID = "Elinor")
