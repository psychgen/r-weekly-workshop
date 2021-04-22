#week 5.R

library(tidyverse)

movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/movies.csv')


movies <- movies %>% 
  mutate_at(vars(matches("_2013")), list(~as.numeric(.) )) %>% 
  mutate_at(vars("test","clean_test","binary"), list(~as.factor(.) )) %>% 
  separate(genre, into = c("genre1","genre2","genre3"), sep=",") %>% 
  mutate(runtime = as.numeric(str_remove(runtime," min|N/A"))) %>% 
  select(imdb,year,clean_test,binary,budget_2013,intgross_2013,domgross_2013,
         decade_code, language,country, runtime, metascore,genre1,genre2,genre3)


## Descriptives

psych::describe(movies)

table(movies$genre1, movies$decade_code)

cor(movies %>% select(matches("_2013"),runtime,metascore), use="pairwise.complete.obs" )

# find unique values of a variable

unique(movies$genre)


## RQ 1: has Bechdel test performance been consistent across decades?

table(movies$decade_code, movies$clean_test, useNA = "ifany")

chisq <- chisq.test(table(movies$decade_code, movies$clean_test, useNA = "ifany"))

chisq$observed

round(chisq$expected)

round(chisq$residuals,3)

#GLM using year
glmodel <- glm(formula= binary ~ year, data = movies, family=binomial())

summary(glmodel)

## RQ 2 does the rxp between budget and rating vary between test passing vs failing movies?

linmodel <- lm(formula = metascore ~ budget_2013*binary , data=movies, na.action = na.exclude )

library(lavaan)

model <- 'metascore ~ budget_2013 '

lav_fit <- sem(model, data=movies, group="binary")

lav_fit_constrained <- sem(model, data=movies, group="binary", group.equal = c("intercepts", "regressions"))


# Create a variable in the dataset with predicted values from the regression model

movies <- movies %>% 
  mutate(predicted = predict(model,movies ),
         residuals = residuals(model),
         observed = intgross_2013)

ggplot(data=movies, aes(x=observed,y=predicted))+
  geom_point()+
  facet_grid(clean_test~decade_code)
