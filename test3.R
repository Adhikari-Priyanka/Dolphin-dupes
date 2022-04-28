rm(list=ls())
library("tidyverse")
{
  old <- read_csv("OLD_og.csv")
  web <- read_csv("web_og.csv")
  
  #cleaning
  #remove row 1 of old2, it has column headings again.
  old2 <-old[2:nrow(old), ]
  
  #select only day month year and species
  old3 <- old2 %>% select(Day, Month, Year, Species)
  web2 <- web %>% select(day, month, year, species_code)
  
  #give everything a unique id
  old4 <- old3 %>% mutate(old_id = c(1:nrow(old3)))
  web3 <- web2 %>% mutate(web_id = c(1:nrow(web2)))
  
  #Remove NA in web
  web_na<- which(is.na(web3$species_code))
  web4<- web3[-web_na,]
  
  #Rename columns names in web
  web5 <- web4 %>% rename(Month = month, Day = day, 
                          Month = month, Species = species_code)
  
  #Cut down old
  old5 <- old4 %>%filter(between(Year, 2000, 2008))
  unique(old5$Year)
  
  #Remove variables not required
  rm(web, web2, web3, web4,
     old, old2, old3, old4,
     web_na)
  
}#load and trim
#result: web4 and old4

##what does my data look like?
overview <- function(x){
  for (i in (1:ncol(x))){
    find <- x %>% pull(i)
    print(paste("range of ", colnames(x[,i])))
    print(range(find))
    print(paste("unique in ",colnames(x[,i])))
    if(length(unique(find))<50){print(sort(unique(find)))}
    else{print(sort(unique(find))[1:10])}
    print(paste("na values ", which(is.na(x[, i]))))
  }}

overview(old5)
overview(web5)

#test 3
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
    old_id_matches <- filtered_old %>% pull(5)
    web_id_dupe <- (web %>% pull(5))[i]
    res <- data.frame(Web_ids_of_dupes= rep(web_id_dupe, length(old_id_matches)), 
                      old_id_of_matches= (old_id_matches+correction))
    #Binding the matched ids in one csv file
    result <- rbind(result, res)
  }
  write.csv(result, "Dupes_and_matches.csv")
  final_result <<- result
}

#plug in the db
#web 11852 rows, old 44193 rows#
system.time(check_dupes(web=web5[1:1000,], old= old5, correction=1))

#checking final_result
i=floor(runif(1, 1, nrow(final_result)))
web[ final_result[i,1], 8:13]
old[ final_result[i, 2], 9:14]
