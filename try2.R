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

#fix NR and NA
NR_remove <- function(sheet){
  sheet1 <- data.frame()
  for (i in 1:nrow(sheet)){
    for (j in 1:ncol(sheet)){
      if (sheet[i,j] == "NR" ||
          sheet[i,j] == "NA" ||
          sheet[i,j] == "na"){
        sheet1 <- sheet[-i,]
      }
    }
  }
  sheet2 <<- sheet1
}

NR_remove(web6)
sheet2

NR_remove(old6)
old7 <- sheet1


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

#Checking other stuff as well
overview(old7[,1:7])
overview(web7[, 1:6])

#only checking time
overview(old7[,8:11])
overview(web7[, 7:8])

#test 5
#define function
check_dupes <- function(web, old, correction){
  #Initialize empty data frame
  result <- data.frame()
  web <- web %>% mutate("Duplicate" = rep(0, nrow(web)))
  for (i in 1:nrow(web)){
    #pull web species, year, month and day
    web_species <- (web %>% pull(4))[i]
    web_year <- (web %>% pull(3))[i]
    web_month <- (web %>% pull(2))[i]
    web_day <- (web %>% pull(1))[i]
    
    #tibble of potential old matches
    filtered_old <- old %>% filter(Species == web_species&
                                     Year == web_year &
                                     Month == web_month &
                                     Day == web_day)
    filtered_old <- filtered_old %>% mutate("Match" = rep(0, nrow(filtered_old)))
    
    #only process with the time thing if there are actually matches
    if (nrow(filtered_old) > 0){
      #extract web time
      web_hour <- (web %>% pull(7))[i]
      web_min <- (web %>% pull(8))[i]
      #put extra column in filtered_old for what kinda match
      nm <- c() #rows in filtered_old that are not matches
      for (j in 1:nrow(filtered_old)){
        old_start_hour <- (filtered_old %>% pull(8))[j]
        #old_start_min <- (filtered_old %>% pull(9))[j]
        old_end_hour <- (filtered_old %>% pull(10))[j]
        #old_end_min <- (filtered_old %>% pull(11))[j]
        
        #first see if old_start_hour is the same as old_end_hour
        if(old_start_hour == old_end_hour){
          if (web_hour == old_start_hour){
            filtered_old$Match[j] = "Exact"}}
        
        else if (  (web_hour > old_start_hour) & (web_hour < old_end_hour)  ){
          filtered_old$Match[j] = "Range"}
        
        else {
          filtered_old$Match[j] = "No match"
          nm <- c(nm, j)}
      }
      #remove non matches from filtered_old
      filtered_old <- filtered_old[ -nm ,]
      #Results#
      #CSV with web_id_dupes and old_id_matches
      old_id_matches <- (filtered_old  %>% pull(7))
      old_match_type <- (filtered_old  %>% pull(12))
      web_id_dupe <- (web %>% pull(6))[i]
      res <- data.frame(Web_ids_of_dupes= web_id_dupe,
                        Old_id_of_matches= (as.numeric(old_id_matches)+correction),
                        Match_type= old_match_type)
      result <- rbind(result, res)
    }
    

  }
  write.csv(result, "test5_Dupes_and_matches.csv")
  final_result <<- result
}

#plug in the db
#web 11852 rows, old 44193 rows#
system.time(check_dupes(web=web6[880:890,], old= old6, correction=1))

#check time
final_result <- read.csv("test3_Dupes_and_matches.csv")
final_result <- final_result %>% mutate(n=rep(0,nrow(final_result)))
for( i in 1:nrow(final_result)){
  w <- final_result[i,2]
  o <- final_result[i,3]
  web_time <- (web5 %>% pull(5))[w]
  old_time_end <-(old5 %>% pull(6))[o]
  old_time_start <-(old5 %>% pull(5))[o]

  #check if time is same
  if (web_time == old_time_end | web_time == old_time_start){
    final_result$n[i] <- "TRUE"
  }
  
  
}
write.csv(final_result, "test4.csv")



#finally, add a row in web_og of dupes
web_og <- read.csv("web_og.csv")
dupe_id <- unique(final_result[,1]) ##

dupe_col <- function(web, dupe_where){
  complt <- web %>% mutate(Duplicate = rep(0, nrow(web)))
  for (i in dupe_where){
    complt$Duplicate[i] ="TRUE"
  }
  ans <<- complt
  write.csv(ans, "test4_DONE.csv")
}
dupe_col(web=web_og, dupe_where=dupe_id)

#is this in line with the manual check?
mn_dupe <- which(web$DUP== "yes")
mn_dupe %in% dupe_id

#checking final_result in r
i=floor(runif(1, 1, nrow(final_result)))
i=which(final_result==883)
web[ final_result[i,1], 8:13]
old[ final_result[i, 2], 9:14]

######fix time
##check if dupes off  by 1 row. just in case
