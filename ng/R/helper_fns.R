
# helper functions

#
is_past <- function(date) as.Date(date) < Sys.Date()

#
abbr <- function(x){substr(x, 1, 3)}

#
date_to_weekday <<- function(date) {format(as.Date(date), format = "%a")}