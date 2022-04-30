rm(list=ls())
library("tidyverse")
library("lubridate")
{
  old <- read_csv("OLD_og.csv")
  web <- read_csv("web_og.csv")
  
  #cleaning
  #remove row 1 of old2, it has column headings again.
  old2 <-old[2:nrow(old), ]
  
  #select only day month year and species
  old3 <- old2 %>% select(Day, Month, Year, Species,Time_start, Time_end)
  web2 <- web %>% select(day, month, year, species_code, Time)
  
  #give everything a unique id
  old4 <- old3 %>% mutate(old_id = c(1:nrow(old3)))
  web3 <- web2 %>% mutate(web_id = c(1:nrow(web2)))
  
  #Remove NA in web
  web_na<- which(is.na(web3$species_code))
  web4<- web3[-web_na,]
  
  #Rename columns names in web
  web5 <- web4 %>% rename(Year = year, Day = day, 
                          Month = month, Species = species_code)
  
  #Cut down old
  old5 <- old4 %>%filter(between(Year, 2000, 2008))
  unique(old5$Year)
  
  #Remove variables not required
  rm(web, web2, web3, web4,
     old, old2, old3, old4,
     web_na)
  
}#load and trim
#result: web5 and old5

#fix time
{
  web_time <- web5 %>% pull(5)
  old_time_end <- old5 %>% pull(6)
  old_time_start <- old5 %>% pull(5)
  
  #extract hours and minutes
  #####ensure time is in hh:mm:ss format in a 24 hour clock
  #bind this to web5 and old5 and rename?
  web6 <- cbind(web5, 
                "Hour" = hour(hms(web_time)),
                "Min"= minute(hms(web_time)))
  old6 <- cbind(old5, 
                "Start_hour" =hour(hms(old_time_start)),  
                "Start_min" =minute(hms(old_time_start)),
                "End_hour" =hour(hms(old_time_end)), 
                "End_min" =minute(hms(old_time_end)))
  
  
  #remove not required
  rm(web_time, old_time_start, old_time_end)
  rm(web5, old5)
}

#Result: old6 and web6

##what does my data look like?
overview <- function(x){
  for (i in (1:ncol(x))){
    find <- x %>% pull(i)
    print(paste("range of ", colnames(x[,i])))
    print(range(find))
    print(paste("unique in ",colnames(x[,i])))
    if(length(unique(find))<50){print(sort(unique(find)))}
    else{print(sort(unique(find))[1:10])
      print("more than 50 values")}
    print(paste("na values ", which(is.na(x[, i]))))
  }}
glimpse(web6)
glimpse(web7)


#test 4
#define function
check_dupes <- function(web, old, correction){
  #Initialize empty data frame
  result <- data.frame()
  #For every row of web db, filter old db with the same year, month, day and species
  for (i in 1:nrow(web)){
    web_species <- (web %>% pull(4))[i]
    web_year <- (web %>% pull(3))[i]
    web_month <- (web %>% pull(2))[i]
    web_day <- (web %>% pull(1))[i]
    #filtering old based on a specific web entry
    filtered_old <- old %>% filter(Species == web_species&
                                     Year == web_year &
                                     Month == web_month &
                                     Day == web_day)
    #Results#
    #CSV file with web_id_dupes and old_id_matches
    old_id_matches <- filtered_old %>% pull(7)
    web_id_dupe <- (web %>% pull(6))[i]
    #add time hours and minutes
    web_hour <- (web %>% pull(7))[i]
    web_min <- (web %>% pull(8))[i]
    old_srt_hour <- filtered_old %>% pull(8)  ###check this. what is the time thingie in filtered?
    old_srt_min <- filtered_old %>% pull(9)
    old_end_hour <- filtered_old %>% pull(10)
    old_end_min <- filtered_old %>% pull(11) ###checkk
    #bring it all together
    res <- data.frame(Web_ids_of_dupes= rep(web_id_dupe, length(old_id_matches)),
                      web_hours= rep(web_hour, length(old_id_matches)),
                      web_mins= rep(web_min, length(old_id_matches)),
                      old_id_of_matches= (as.numeric(old_id_matches)+correction),
                      old_start_hour= old_srt_hour,
                      old_start_min= old_srt_min,
                      old_end_hour= old_end_hour,
                      old_end_min= old_end_min
    )
    #Binding the matched ids in one csv file
    result <- rbind(result, res)
  }
  write.csv(result, "test4_Dupes_and_matches.csv")
  final_result <<- result
}
#the only time correction i have done is adding hours and minutes to the final_results table
#final_results table should now have web_id, old_id, and times

#plug in the db
#web 11852 rows, old 44193 rows#
check_dupes_time <- system.time(check_dupes(web=web6, old= old6, correction=1))

#to the time thing man idk anymore
#extract web time
final_result <- read.csv("test4_Dupes_and_matches.csv")#if not already loaded
time_thing <- function(res){
  #make a new column for types of matches
  res <- res %>% mutate("match_type"= rep(0,nrow(res)))
  res <- res %>% mutate("y_n"= rep(0,nrow(res)))
  for (i in 1:nrow(res)){
    #pull web time
    web_hour <- (res %>% pull(web_hours))[i]
    web_min <- (res %>% pull(web_mins))[i]
    #convert to mins
    web_time <- (web_hour*60)+web_min
    
    #pull old_time
    old_start_hour <- (res %>% pull(old_start_hour))[i]
    old_start_min <- (res %>% pull(old_start_min))[i]
    old_start <- (old_start_hour*60)+old_start_min#convert to mins
    old_end_hour <- (res %>% pull(old_end_hour))[i]
    old_end_min <- (res %>% pull(old_end_min))[i]
    old_end <- (old_end_hour*60)+old_end_min#convert to mins
    
    if(old_start == old_end){# old_st == old_end
      if(web_time == old_start){#is web_time the same?
        res$match_type[i]  = "Exact match"
        res$y_n[i]="yes"}
      else {#web time is different
        res$match_type[i]  = "No match"
        res$y_n[i]="no"}    }
    else { #old_st != old_end
      if ( (web_time>=old_start)  &  (web_time<=old_end) ){#is web in range
        res$match_type[i]  = "Within range"
        res$y_n[i]="maybe"}
      else {#web is not in range
        res$match_type[i]  = "No match"
        res$y_n[i]="no"}    }
    
  }
  
  
  #filter time_result by matches
  time_result <<- res
  write.csv(time_result, "test4_time_result.csv")
}

#call the function 
time_thing_time <- system.time(time_thing(res= final_result))

time_result <- read.csv("test4_time_result.csv")

#what do time_results look like?
length(which(time_result$y_n=="yes"))
length(which(time_result$y_n=="no"))
length(which(time_result$y_n=="maybe"))
length(unique(time_result$Web_ids_of_dupes))
nrow(time_result)

#filter by yes and maybe
fltr <- time_result %>% filter(y_n == "yes" | y_n == "maybe")
write.csv( fltr,"test4_DONE.csv")

#finally, add a row in web_og of dupes
web_og <- read.csv("web_og.csv")
DONE_web_og <- web_og %>% mutate(web_id=1:nrow(web_og)  ,y_n=rep(0, nrow(web_og)))

for (i in 1:nrow(fltr)){
  if (fltr$y_n[i] == "yes"){ #if exact match
    DONE_web_og[fltr$Web_ids_of_dupes[i],30] = "yes"
  }
  
  else if (fltr$y_n[i] == "maybe") { #if maybe
    DONE_web_og[fltr$Web_ids_of_dupes[i],30] = "maybe"
  }
  
}

write.csv(DONE_web_og, "test4_DONE_web_og.csv")




#check if web_id is repeated?
n <- data.frame(table(fltr$Web_ids_of_dupes))
repeats <- (fltr[ fltr$Web_ids_of_dupes %in% n$Var1[n$Freq>1] ,c(1,4)])
repeats[1:5,]
write.csv(repeats, "test4_repeat-duplicates.csv")


#is this in line with the manual check?
mn_dupe <- which(web_og$ï..DUP== "yes")
length(mn_dupe)
length(which(mn_dupe %in% time_result$Web_ids_of_dupes))



#checking final_result in r
i=floor(runif(1, 1, nrow(final_result)))
i=which(final_result==883)
web[ final_result[i,1], 8:13]
old[ final_result[i, 2], 9:14]





