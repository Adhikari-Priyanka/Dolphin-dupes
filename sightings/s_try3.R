rm(list=ls())

setwd("sightings")
#setwd("C://Users//priya//Documents//SWF//Dolphin-dupes//sightings")
library(tidyverse)
library(lubridate)
md <- read_csv("md_s.csv")
old <- read_csv("old_s.csv")

{##First row is column names in lowercase
  old <- old %>% rename(day= Day, month= Month, year= Year,
                        species=Species,est_grp=Best_est_group,
                        time_start=Time_start, time_end=Time_end,
                        sighting_id =Sighting_ID)
  md <- md %>% rename(day= Day, month= Month, year= Year,
                      species=Species,est_grp=Best_est_group,
                      time_start=Time_start, time_end=Time_end,
                      sighting_id =Sighting_ID)
  
  
  clean_old <- function(data){
    data <- data %>% select(sighting_id,day, month, year, 
                            species, est_grp,
                            time_end, time_start) #select only these columns
    data <- data[-1,]
    data <- data %>% mutate(row_num = c(1:nrow(data))) #add a column containing row numbers
    data <- na.omit(data) #remove rows with na values
    data$time_end = ( hour(hms(data$time_end))*60 + minute(hms(data$time_end))   )
    data$time_start = ( hour(hms(data$time_start))*60 + minute(hms(data$time_start))   )
    return(data)
  }
  old1 <- clean_old(old)
  md1 <- clean_old(md)
  
  #narrow down to rows with matching sighting_id
  old_row <- which(old1$sighting_id %in% md1$sighting_id)
  old2 <- old1[old_row,]
  md_row <- which(md1$sighting_id %in% old1$sighting_id)
  md2 <- md1[md_row,]
  
  rm(old, md, old1, md1, clean_old)
  write.csv(old2, "old2_s.csv")
  write.csv(md2, "md2_s.csv")
  
}


#define function
check_dupes <- function(web, old){
  #Initialize empty data frame
  result <- data.frame()
  #For every row of web db, filter old db with the same year, month, day and species
  for (i in 1:nrow(web)){
    #pull year, month, day and species
    web_species <- (web %>% pull(species))[i]
    web_est <- (web %>% pull(est_grp))[i]
    web_year <- (web %>% pull(year))[i]
    web_month <- (web %>% pull(month))[i]
    web_day <- (web %>% pull(day))[i]
    #filtering old based on a specific web entry
    filtered_old <- old %>% filter(species == web_species&
                                     est_grp == web_est &
                                     year == web_year &
                                     month == web_month &
                                     day == web_day)
    
    #Results#
    if(nrow(filtered_old) != 0){
      #pull times and ids
      old_id_matches <- filtered_old %>% pull(row_num)
      old_start <- filtered_old %>% pull(time_start)
      old_end <- filtered_old %>% pull(time_end)
      web_id_dupe <- (web %>% pull(row_num))[i]
      web_start <- (web %>% pull(time_start))[i]
      web_end <- (web %>% pull(time_end))[i]
      #CSV file with web_id_dupes and old_id_matches
      res <- data.frame(Web_ids_of_dupes= rep(web_id_dupe, length(old_id_matches)),
                        web_start= rep(web_start, length(old_id_matches)),
                        web_end= rep(web_end, length(old_id_matches)),
                        old_id_of_matches= old_id_matches,
                        old_start= old_start,
                        old_end= old_end
      )
      #Binding the matched ids in one csv file
      result <- rbind(result, res)  
    }
    
  }
  
  
  ##time thing
  #make a new column for types of matches
  result <- result %>% mutate("match_type"= rep(0,nrow(result)),
                              "y_n"= rep(0,nrow(result)))
  for (i in 1:nrow(result)){
    #pull web time
    web_start <- (result %>% pull(web_start))[i]
    web_end <- (result %>% pull(web_end))[i]
    
    #pull old_time
    old_start <- (result %>% pull(old_start))[i]
    old_end <- (result %>% pull(old_end))[i]
    
    if(old_start == web_start | old_end == web_end){result$y_n[i]="yes"}
    
    else {result$y_n[i]="no"} 
    
  }
  return(result)
}


#plug in the db
#web 11852 rows, old 44193 rows
check_dupes_time <- system.time(
  res <- check_dupes(web=old2, old= md2))

write.csv(res,  "FINAL_matches_and_dupes.csv")


#res <- read_csv("test3_matches_and_dupes.csv")
old_og <- read.csv("old_s.csv")


in_og <- function(res, og){
  res <- res %>% filter(y_n == "yes") #filter by yes
  #finally, add a column in web_og of dupes
  DONE_web_og <- og %>% mutate(y_n=rep(0, nrow(og)))
  for (i in 1:nrow(res)){
    if (res$y_n[i] == "yes"){ #if exact match
      DONE_web_og$y_n[res$Web_ids_of_dupes[i]+1] = "yes"}
  }
  return( DONE_web_og)
  
}

done <- in_og(res= res, og= old_og)
write.csv(done, "FINAL_DONE_old_og.csv")


#check
res <- read_csv("FINAL_matches_and_dupes.csv")
old2 <- read_csv("old2_s.csv")
md2 <- read_csv("md2_s.csv")
res <- res %>% filter(y_n=="yes")
test <- floor(runif(5, min=1, max=nrow(res)))
web_ids <- res$Web_ids_of_dupes[test]
old_ids <- res$old_id_of_matches[test]






