
database <<- "room_avail_test.db"

input <- list(all_cells_selected = matrix(c(1,4), nrow = 1), # 9am_10am
              checkbox_ampm = "ampm",
              date_search = "2020-07-15")

room_table <- loadData(database, 'new_room_status')

# empty table
create_candidate_table(input,
                       user_ID = "Simon")

# Booked
input$all_cells_selected <- matrix(c(1,11), nrow = 1) # 4pm-5pm
create_candidate_table(input,
                       user_ID = "Simon")

# empty table
input$date_search <- "2020-07-14"
input$all_cells_selected <- matrix(c(1,4), nrow = 1)
create_candidate_table(input,
                       user_ID = "Elinor")

## multiple times

input$all_cells_selected <- matrix(c(1,6,
                                     1,7),
                                   ncol = 2,
                                   byrow = TRUE) # 11am-1pm
# empty table
input$date_search <- "2020-07-14"
create_candidate_table(input,
                       user_ID = "Simon")

# Booked
input$date_search <- "2020-07-17"
create_candidate_table(input,
                       user_ID = "Simon")


## multiple dates
##TODO:

input$all_cells_selected <- matrix(c(1,6,
                                     1,7,
                                     2,4),
                                   ncol = 2,
                                   byrow = TRUE) # 11am-1pm, 9am-10am
create_candidate_table(input,
                       user_ID = "Simon")
