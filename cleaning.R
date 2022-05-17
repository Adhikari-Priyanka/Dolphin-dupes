rm(list=ls())
library(tidyverse)
library(lubridate)

#setwd("C://Users//priya//Documents//SWF//Dolphin-dupes")

old <- read_csv("OLD_og.csv")
web <- read_csv("web_og.csv")

##First row is column names in lowercase
old <- old[-1,]
old <- old %>% rename(day= Day, month= Month, year= Year,
                      time_start=Time_start, time_end=Time_end, species = Species)
web <-web %>% rename(day= day, month= month, year= year, species= species_code, time1=Time)

clean_web <- function(data){
  data <- data %>% select(day, month, year, species, time1) #select only these columns
  data <- data %>% mutate(web_row_num = c(1:nrow(data))) #add a column containing row numbers
  #Make a new column of time in minute form
  data <- data %>% mutate(web_time= (hour(hms(time1))*60)+minute(hms(time1)))
  data <- na.omit(data)
  return(data)
}
web1 <- clean_web(web)

clean_old <- function(data){
  data <- data %>% select(day, month, year, species, time_end, time_start) #select only these columns
  data <- data %>% mutate(old_row_num = c(1:nrow(data))) #add a column containing row numbers
  data <- data %>%filter(between(year, 2000, 2008)) #Filter down old

  
  #Make a new column of time in minute form
  data <- data %>% mutate(old_start= 
                            (hour(hms(time_start))*60)+minute(hms(time_start)) )
  data <- data %>% mutate(old_end= 
                            (hour(hms(time_end))*60)+minute(hms(time_end)) )
  data <- na.omit(data) #remove rows with na values
  return(data)
}
old1 <- clean_old(old)

rm(old, web, clean_old, clean_web)
#Ensure excel sheet:
##First row is column names in lowercase
##Time is in the format hh:mm:ss or hh:mm under column name time_start or time_end or simply time1
###try to not name the column 'time' since it might confuse R.
##Date is separated into different columns of day, month and year
##Month is in numeric format, year entered as 20xx
##Species names to be entered in 3-letter code (column name: species)

write.csv(old1, "old1.csv")
write.csv(web1, "web1.csv")


