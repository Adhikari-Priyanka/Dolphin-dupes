library(tidyverse)
library(lubridate)

old <- read_csv("OLD_og.csv")
web <- read_csv("web_og.csv")


clean <- function(data, file_number){
  dataa <- data %>% select(day, month, year, species, time1) #select only these columns
  data <- data %>% mutate(row_num = c(1:nrow(data1))) #add a column containing row numbers
  data <- na.omit(data)
  write.csv(data3, paste("clean", file_number,".csv") )
  
  #Make a new column of time in minute form
  data <- data %>% mutate(time_mins= 
                    (hour(hms(time1))*60)+minute(hms(time1)) )
}

#Ensure excel sheet:
##First row is column names in lowercase
##Time is in the format hh:mm:ss or hh:mm under column name time_start or time_end or simply time1
###try to not name the column 'time' since it might confuse R.
##Date is separated into different columns of day, month and year
##Month is in numeric format, year entered as 20xx
##Species names to be entered in 3-letter code (column name: species)

#Filter down old
old5 <- old4 %>%filter(between(Year, 2000, 2008))

