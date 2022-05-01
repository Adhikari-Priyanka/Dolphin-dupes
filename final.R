#web_id
#web_time
#old_id
#old_start
#old_end

#define function
check_dupes <- function(web, old){
  #Initialize empty data frame
  res_filter <- data.frame()
  #For every row of web db, filter old db with the same year, month, day and species
  for (i in 1:nrow(web)){
    #pull year, month, day and species
    web_species <- (web %>% pull(species))[i]
    web_year <- (web %>% pull(year))[i]
    web_month <- (web %>% pull(month))[i]
    web_day <- (web %>% pull(day))[i]
    #filtering old based on a specific web entry
    filtered_old <- old %>% filter(Species == web_species&
                                     Year == web_year &
                                     Month == web_month &
                                     Day == web_day)
    
    #Results#
    #pull times and ids
    old_id_matches <- filtered_old %>% pull(old_id)
    old_start <- filtered_old %>% pull(old_start)
    old_end <- filtered_old %>% pull(old_end)
    web_id_dupe <- (web %>% pull(web_id))[i]
    web_time <- (web %>% pull(web_time))[i]
    #CSV file with web_id_dupes and old_id_matches
    res <- data.frame(Web_ids_of_dupes= rep(web_id_dupe, length(old_id_matches)),
                      web_time= rep(web_time, length(old_id_matches)),
                      old_id_of_matches= old_id_matches,
                      old_start= old_start, 
                      old_end= old_end)
    #Binding the matched ids in one csv file
    res_filter <- rbind(res_filter, res)
  }
  
  #write csv of all matches
  #write.csv(result, "test4_Dupes_and_matches.csv")
  
  ##time thing
  #make a new column for types of matches
  res_filter <- res_filter %>% mutate("match_type"= rep(0,nrow(res_filter)),
                                      "y_n"= rep(0,nrow(res_filter)))
  for (i in 1:nrow(res_filter)){
    #pull web time
    web_time <- (res_filter %>% pull(web_time))[i]

    #pull old_time
    old_start <- (res_filter %>% pull(old_start))[i]
    old_end <- (res_filter %>% pull(old_end))[i]

    if(old_start == old_end){# old_st == old_end
      if(web_time == old_start){#is web_time the same?
        match_type[i]  = "Exact match"
        y_n[i]="yes"}
      else {#web time is different
        match_type[i]  = "No match"
        y_n[i]="no"}    }
    else { #old_st != old_end
      if ( (web_time>=old_start)  &  (web_time<=old_end) ){#is web in range
        match_type[i]  = "Within range"
        y_n[i]="maybe"}
      else {#web is not in range
        match_type[i]  = "No match"
        y_n[i]="no"}    }

  }
  #filter time_result by matches
  res_time <<- res_filter
  write.csv(res_time, "matches_and_dupes.csv")
  
}

#plug in the db
#web 11852 rows, old 44193 rows
check_dupes(web=clean_web6, old= clean_old)
check_dupes_time <- system.time(check_dupes(web=web6, old= old6))

res_time <- read_csv("matches_and_dupes.csv")
#filter by yes and maybe
fltr <- res_time %>% filter(y_n == "yes" | y_n == "maybe")
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

write.csv(DONE_web_og, "DONE_web_og.csv")


