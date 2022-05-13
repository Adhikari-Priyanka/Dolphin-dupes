rm(list=ls())
setwd("web")
#setwd("C://Users//priya//Documents//SWF//Dolphin-dupes//web")
library(tidyverse)
library(lubridate)
md <- read.csv("md_w.csv",stringsAsFactors=F, na.strings=c(NA,"NA"," NA"))
web <- read.csv("web_w.csv",stringsAsFactors=F, na.strings=c(NA,"NA"," NA"))

{
  md <- md %>% rename(day= Day, month= Month, year= Year,
                      time_start=Time_start, time_end=Time_end,
                      total=Best_est_group, species= Species)
  web <-web %>% rename(day= day, month= month, year= year, 
                       species= species_code, web_time=Time,
                       total= total)
  
  
  clean_old <- function(data){
    data <- data %>% 
      select(day, month, year, time_end, time_start,total, species)%>% 
      mutate(row_num = c(1:nrow(data))) #add a column containing row numbers
    data <- na.omit(data) #remove rows with na values
    data$time_end = (hour(hms(data$time_end))*60 + minute(hms(data$time_end)))
    data$time_start = (hour(hms(data$time_start))*60 + minute(hms(data$time_start)))
    data <- na.omit(data) #remove rows with na values
    return(data)
  }
  md1 <- clean_old(md)
  clean_web <- function(data){
    data <- data %>% 
      select(day, month, year, species, web_time, total) %>% 
      mutate(web_row_num = c(1:nrow(data))) #add a column containing row numbers
    #Make a new column of time in minute form
    data <- data %>% mutate(web_time= (hour(hms(web_time))*60)+minute(hms(web_time)))
    data <- na.omit(data)
    return(data)
  }
  web1 <- clean_web(web)
  
  rm(web, md, clean_old, clean_web)
  write.csv(web1, "web1_w.csv")
  write.csv(md1, "md1_w.csv")
}

md1 <- md1 %>% 
  filter(year != "NR") %>% 
  filter(year >= 2000 | year <=2008)

write.csv(md1, "md1_w.csv")

md1 <- read.csv("md1_w.csv")
web1 <- read.csv("web1_w.csv")

#define function
check_dupes <- function(web, old){
  #Initialize empty data frame
  result <- data.frame()
  #For every row of web db, filter old db with the same year, month, day and species
  for (i in 1:nrow(web)){
    #pull year, month, day and species
    web_total <- (web %>% pull(total))[i]
    web_species <- (web %>% pull(species))[i]
    web_year <- (web %>% pull(year))[i]
    web_month <- (web %>% pull(month))[i]
    web_day <- (web %>% pull(day))[i]
    #filtering old based on a specific web entry
    filtered_old <- old %>% filter(species == web_species&
                                     year == web_year &
                                     month == web_month &
                                     day == web_day &
                                     total == web_total)
    
    #Results#
    #pull times and ids
    old_id_matches <- filtered_old %>% pull(row_num)
    old_start <- filtered_old %>% pull(time_start)
    old_end <- filtered_old %>% pull(time_end)
    web_id_dupe <- (web %>% pull(web_row_num))[i]
    web_time <- (web %>% pull(web_time))[i]
    #CSV file with web_id_dupes and old_id_matches
    res <- data.frame(Web_ids_of_dupes= rep(web_id_dupe, length(old_id_matches)),
                      web_time= rep(web_time, length(old_id_matches)),
                      old_id_of_matches= old_id_matches,
                      old_start= old_start, 
                      old_end= old_end)
    #Binding the matched ids in one csv file
    result <- rbind(result, res)
  }
  
  
  ##time thing
  #make a new column for types of matches
  result <- result %>% mutate("y_n"= rep(0,nrow(result)))
  for (i in 1:nrow(result)){
    #pull web time
    web_time <- (result %>% pull(web_time))[i]
    
    #pull old_time
    old_start <- (result %>% pull(old_start))[i]
    old_end <- (result %>% pull(old_end))[i]
    
    if( web_time == old_start){result$y_n[i]="yes"}
    if (web_time == old_end){result$y_n[i]="yes"}
    
  }
  return(result)
}


#plug in the db
#web 11852 rows, old 44193 rows
res<- check_dupes(web=web1, old= md1)
write.csv(res, "test1_matches_and_dupes.csv")


res <- read_csv("test1_matches_and_dupes.csv")
web_og <- read.csv("web_w.csv")

in_og <- function(res, og){
  res <- res %>% filter(y_n == "yes" | y_n == "maybe") #filter by yes and maybe
  #finally, add a column in web_og of dupes
  DONE_web_og <- og %>% mutate(web_id=1:nrow(og)  ,y_n=rep(0, nrow(og)))
  for (i in 1:nrow(res)){
    if (res$y_n[i] == "yes"){ #if exact match
      DONE_web_og$y_n[res$Web_ids_of_dupes[i]] = "yes"}
    else if (res$y_n[i] == "maybe") { #if maybe
      DONE_web_og$y_n[res$Web_ids_of_dupes[i]] = "maybe"}
  }
  return( DONE_web_og)
  
}

done <- in_og(res= res, og= web_og)


write.csv(done, "test1_DONE_web_w.csv")








n_iter=100

# Initializes the progress bar
pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = n_iter, # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=")   # Character used to create the bar
for(i in 1:n_iter) {
  res<- check_dupes(web=web1[1:5000,], old= md1)
  setTxtProgressBar(pb, i)
}
close(pb) # Close the connection


