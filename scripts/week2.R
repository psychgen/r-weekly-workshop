
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

#' Today's data is [plastic pollution](https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-01-26/readme.md)

plastics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv')

#' Task: put together a general report including some visualisations and a table, specifically:
#' 
#' 1. some visualisation of year-on-year change in plastic pollution - if time compare proportions
#' 2. a summary table of events, volunteers, and total pollution per country
#' 3. some visualisation of the rate at which volunteers cleared pollution across different countries/years
#' 
#' 1. Visualise year-on-year change

# Collapse down country and parent_company to get year summary

plot1dat <- plastics %>% 
  group_by(year) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE ) %>% 
  gather(plastic_type, total, -year, -grand_total,-num_events,-volunteers ) %>% 
  arrange(year)

plot1dat_wide <- plot1dat %>% 
  select(-grand_total:-volunteers) %>% 
  spread(year, total)

# Plot using cleveland dot plot to show change
ggplot(data= plot1dat,
       aes(x=plastic_type, y=total, colour=factor(year)))+
  geom_segment(data =plot1dat_wide,  aes(x=plastic_type, xend=plastic_type, y=`2019`, yend=`2020`)
               , color="grey", size=1.5)+
  geom_point(size=3) +
  coord_flip()+
  theme_minimal()+
  theme(text = element_text(size=14, face = "bold" ))+
  scale_x_discrete("Plastic type")+
  scale_y_continuous("Total",  breaks=c(seq(0,500000,100000)), labels=c("0","100,000","200,000","300,000","400,000","500,000"))
  
#' 2. a summary table of rate of collection per country

plastics %>% 
  select(country,grand_total,volunteers) %>% 
  group_by(country) %>%
  summarise(sum_gt = sum(grand_total, na.rm = TRUE),
            sum_vol = sum(volunteers, na.rm = TRUE)) %>% 
  mutate(rate = sum_gt/sum_vol) %>% 
  arrange(sum_gt,country) %>% 
  rename("Country" = country,
         "Total pollution" = sum_gt) %>% 
  knitr::kable(label = "Table 1: blah blah",digits=3)
  
  
  
  

