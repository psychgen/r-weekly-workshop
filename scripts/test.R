#' title: testing the rmd render functionality
#' 


#' Roxygen comment blah blah
#' 
# Regular R comment blah

library(tidyverse)


head(cars)

knitr::kable(table(cars$speed))

# another regular R comment

#' ## some more roxygen comments
#' 
#' this time spread over multiple 
#' 
#' lines (should start with level 2 heading)
#' 
#' 1. bullet 1   
#' 1. bullet 2
#' 

hist(cars$dist)



