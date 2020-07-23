
# find dates in working week
#
dates_in_next_wk <- function() {
  
  date_to_weekday <- function(date){format(as.Date(date), format = "%a")}
  is_weekend <- function(date) date_to_weekday(date) %in% c("Sun","Sat")
  
  next_wk <- data.frame(date = rep(NA, 5),
                        day = c("Mon", "Tue", "Wed", "Thu", "Fri"))
  
  day <- Sys.Date()
  
  if (is_weekend(Sys.Date())) {
    
    while (is_weekend(day)) { # Monday
      day <- day + 1
    }
    
    next_wk$date <- day + 0:4
    
  } else {
    day <- day + 7
    
    while (!is_weekend(day)) { # Sunday
      day <- day - 1
    }
    
    next_wk$date <- day + 1:5
  }
  
  next_wk
}