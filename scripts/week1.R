
#' First we load the required packages:

library(tidyverse)

#' ### A general note on Rstudio projects
#' 
#' We are in an Rstudio project which means that:
#' 
#' "/data/dataset.csv"
#' 
#' ...is equivalent to...
#' 
#' "C:/user/name/weird/idiosyncratic/directory/listings/project/data/dataset.csv"
#'
#' ### A general note on nesting functions vs using pipes:

a <- head(cars)

b <- cars %>% 
  head()

all.equal(a,b)

a2 <- summary(head(cars))

b2 <- cars %>% 
  head() %>% 
  summary()

all.equal(a2,b2)

#' Today's data is [Tate artists/artworks](https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-01-12/readme.md)

artwork <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-12/artwork.csv')
artists <- readr::read_csv("https://github.com/tategallery/collection/raw/master/artist_data.csv")

#' Task: put together a general report including some visualisations; include something that requires merging the two files
#' 

# Cross-tabulation
table(artists$gender, artists$yearOfBirth, useNA = "ifany")


#Simplify the creditLine and medium variables

artwork_simpl <- artwork %>% 
  mutate(credit_simple = factor(case_when(str_detect(creditLine, "Presented") ~ "Presented",
                                   str_detect(creditLine, "Purchased") ~ "Purchased",
                                   str_detect(creditLine, "Bequeathed") ~ "Bequeathed"))) %>% 
  separate(medium,into= paste0("word",seq(1,10,1)), sep = " ") %>% 
  drop_na(credit_simple)

# Visualise the association between year and acquisition year
ggplot(data = artwork_simpl, aes(x=year, y=acquisitionYear, colour=credit_simple, size=width/100)) +
  geom_point()





 