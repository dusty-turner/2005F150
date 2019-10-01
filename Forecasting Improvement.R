library(tidyverse)
library(lubridate)


# 3 date/time classes are built in to R
# Date -- Base
# POSIXct -- Base
# POSIXlt -- Base

# base r

strptime("October 16, 1984", format = "%B %e, %Y")
strptime("16 October, 1984", format = "%e %B, %Y")
strptime("16-- October, 1984", format = "%e-- %B, %Y")
class(strptime("16-- October, 1984", format = "%e-- %B, %Y"))

birthday = strptime("October 16, 1984", format = "%B %e, %Y")

# lubridate

mdy("June 14, 2018")
dmy("14 June, 2018")
dmy("14-- June, 2018")
class(dmy("14-- June, 2018"))

today = mdy("June 14, 2018")
today = as.POSIXlt(today)

# What class

class(birthday)
class(today)

# notice time zone issue

birthday
today

# fix time zone issue

today = force_tz(today, "America/New_York")

today 
birthday

# explore functions

unlist(birthday)
birthday$mon
month(birthday)
birthday$year
year(birthday)

today - birthday

birthday + hours(4)
birthday + days(4)
today + years(4) + months(9)


## truck stuff

truck = read_csv("FordF150.csv")
head(truck)
skimr::skim(truck)

truck = truck %>%
  filter(!is.na(Gallons))

truck
### Transform Variable Types

# Based off what we've observed, we need to do the following things:  
# - R currently reads the dates as character vectors. We'll need to `mutate` them so R reads them correctly.  
# - R Reads the TotalMiles column as a character so we'll need to `mutate` the column to numeric as well.
# - The notes column designates if there was uncertainty in the data collection.  I'll `mutate` to change this to a numeric 1/0 indicator.

truck = truck %>%
  mutate(FillUpDate = force_tz(dmy(FillUpDate), "America/New_York")) %>%
  mutate(FinishTankDate = force_tz(dmy(FinishTankDate), "America/New_York")) %>%
  mutate(TotalMiles = as.numeric(TotalMiles)) %>%
  mutate(Notes = ifelse(is.na(Notes),0,1))
truck

### Lets look at some descriptive statistics

# Largest Fill Up

truck %>%
  filter(Gallons > 32) %>%
  arrange(desc(Gallons)) %>% 
  print(n = Inf)

# Most MPG

truck %>%
  arrange(desc(MPG)) %>% 
  print(n = 20)

# Least MPG

truck %>%
  arrange(MPG) %>% 
  print(n = 20)

# View My Travel By Fillup

truck %>%
  # filter(FillUpDate>"2014-01-15") %>%
  # filter(FillUpDate<"2015-01-01") %>%
  ggplot(aes(x = FillUpDate, y = TotalMiles)) +
  geom_segment(aes(x=FillUpDate, xend=FinishTankDate, yend = TotalMiles)) +
  ggtitle("Distance vs Time")

# View My Travel By MPG

truck %>%
  # filter(FillUpDate>"2014-01-15") %>%
  # filter(FillUpDate<"2015-01-01") %>%
  ggplot(aes(x = FillUpDate, y = MPG, color = Notes)) +
  geom_point()+
  geom_line() +
  # geom_segment(aes(x=FillUpDate, xend=FinishTankDate, yend = MPG)) +
  ggtitle("MPG vs Time")




knitr::opts_chunk$set(message = FALSE, warning = FALSE, fig.show = "animate")
install.packages("gganimate")
devtools::install_github("ImageMagick/ImageMagick")
# Example from https://github.com/dgrtwo/gganimate

library(gapminder)
library(ggplot2)
theme_set(theme_bw())

p <- ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, color = continent, frame = year)) +
  geom_point() +
  scale_x_log10()

library(gganimate)

gganimate(p)


install.packages("ImageMagick")



########################
library(tidyverse)
library(lubridate)
library(forecast)
library(tseries)

truck = read_csv("FordF150.csv")

truck =
truck %>%
janitor::clean_names(case = "snake")

truck = truck %>%
filter(!is.na(gallons))

truck = truck %>%
mutate(fill_up_date = force_tz(dmy(fill_up_date), "America/New_York")) %>%
mutate(finish_tank_date = force_tz(dmy(finish_tank_date), "America/New_York")) %>%
mutate(total_miles = as.numeric(total_miles)) %>%
mutate(notes = ifelse(is.na(notes),0,1))


helper = truck %>%
  mutate(month = month(fill_up_date)) %>%
  mutate(year = year(fill_up_date)) %>%
  group_by(month, year) %>%
  summarise(gallons = sum(gallons), mpg = mean(mpg)) %>%
  arrange(year, month)

startday = mdy("2-2-2006")
endday = mdy("6-2-2018")
vec = startday + months(0:149)

together = tibble(date = vec)  %>%
  mutate(month = month(date)) %>%
  mutate(year = year(date))

mod = full_join(together, helper) %>%
  mutate(gallons = ifelse(is.na(gallons),0,gallons)) %>%
  mutate(mpg = ifelse(is.na(mpg),0,mpg)) %>%
  select(gallons) %>%
  ts(start = 06, frequency = 12) 

class(mod)

mod %>% adf.test()
library(forecast)
mod %>%
  # decompose() %>%
  autoplot()

mod %>%
# holt(h=36) %>% ## doulbe exponential smoothing -- estimates slope too
# ses(h=36) %>%  ## constants processes
hw(h=36, seasonal = "additive") %>% ## seasonal double exponential smoothing
  autoplot()

mod2 = full_join(together, helper) %>%
  mutate(gallons = ifelse(is.na(gallons),0,gallons)) %>%
  mutate(mpg = ifelse(is.na(mpg),0,mpg)) %>%
  select(mpg) %>%
  ts(start = 06, frequency = 12) 

class(mod2)

mod2 %>% adf.test()

mod2 %>%
  decompose() %>%
  autoplot()

mod2 %>%
# holt(h=36) %>% ## doulbe exponential smoothing -- estimates slope too
# ses(h=36) %>%  ## constants processes
hw(h=36, seasonal = "additive") %>% ## seasonal double exponential smoothing
  autoplot()

mod2 %>%
  # holt(h=36) %>% ## doulbe exponential smoothing -- estimates slope too
  # ses(h=36) %>%  ## constants processes
  hw(h=36, seasonal = "additive")


test=
mod %>%
  # holt(h=36) %>% ## doulbe exponential smoothing -- estimates slope too
  # ses(h=36) %>%  ## constants processes
  hw(h=36, seasonal = "additive")  ## seasonal double exponential smoothing
  # autoplot()

str(test)
test
((test$upper[,2]-
test$lower[,2])/2)+test$lower[,2]
