#web_id
#web_time
#old_id
#old_start
#old_end

#define function
check_dupes <- function(web, old){
  #Initialize empty data frame
  result <- data.frame()
  #For every row of web db, filter old db with the same year, month, day and species
  for (i in 1:nrow(web)){
    #pull year, month, day and species
    web_species <- (web %>% pull(species))[i]
    web_year <- (web %>% pull(year))[i]
    web_month <- (web %>% pull(month))[i]
    web_day <- (web %>% pull(day))[i]
    #filtering old based on a specific web entry
    filtered_old <- old %>% filter(species == web_species&
                                     year == web_year &
                                     month == web_month &
                                     day == web_day)
    
    #Results#
    #pull times and ids
    old_id_matches <- filtered_old %>% pull(old_row_num)
    old_start <- filtered_old %>% pull(old_start)
    old_end <- filtered_old %>% pull(old_end)
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
  result <- result %>% mutate("match_type"= rep(0,nrow(result)),
                                      "y_n"= rep(0,nrow(result)))
  for (i in 1:nrow(result)){
    #pull web time
    web_time <- (result %>% pull(web_time))[i]

    #pull old_time
    old_start <- (result %>% pull(old_start))[i]
    old_end <- (result %>% pull(old_end))[i]

    if(old_start == old_end){# old_st == old_end
      if(web_time == old_start){#is web_time the same?
        result$match_type[i]  = "Exact match"
        result$y_n[i]="yes"}
      else {#web time is different
        result$match_type[i]  = "No match"
        result$y_n[i]="no"}    }
    else { #old_st != old_end
      if ( (web_time==old_start)  |  (web_time==old_end) ){#is web in range
        result$match_type[i]  = "Either match"
        result$y_n[i]="maybe"}
      else if ( (web_time>old_start)  &  (web_time<old_end) ){#is web in range
        result$match_type[i]  = "Within range"
        result$y_n[i]="maybe"}
      else {#web is not in range
        result$match_type[i]  = "No match"
        result$y_n[i]="no"}    }

  }
  return(result)
}


#plug in the db
#web 11852 rows, old 44193 rows
write.csv(check_dupes(web=web1, old= old1), "test5_matches_and_dupes.csv")

check_dupes_time <- system.time(check_dupes(web=web1, old= old1))

res <- read_csv("test5_matches_and_dupes.csv")
web_og <- read.csv("web_og.csv")

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

write.csv(done, "test5_web_og.csv")



#figure out maybes
#filter by maybe. 
res <- read_csv("FINAL_matches_and_dupes.csv")
res1 <- res %>% select(Web_ids_of_dupes, old_id_of_matches,y_n) %>% 
  filter(y_n == "maybe")
web_og <- read.csv("web_og.csv")
old_og <- read.csv("OLD_og.csv")





