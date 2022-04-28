rm(list=ls())

setwd("C:\\SWF\\try_dup\\PAD_try")
install.packages("tidyverse")
library("tidyverse")

old <- read_csv("OLD_og.csv")
web <- read_csv("web_og.csv")

#select only day month year and species
old2 <- old %>% select(Day, Month, Year, Species)
web2 <- web %>% select(day, month, year, species_code)

#cleaning
######do this BEFORE!
#remove row 1 of old2, it has column headings again.
old3 <-old2[2:nrow(old2), ]

#tell r thatD, M and Y are int
old4 <- old3 %>% mutate(Day= as.integer(Day), 
             Month= as.integer(Month), ###why not working?
             Year= as.integer(Year))
web3 <- web2 %>% mutate(day= as.integer(day), #change to int data type
             month= as.integer(month), 
             year= as.integer(year),
             id=1:nrow(web2) #added a id- perhaps better added in th excel sheet
             )

##test 2
o<-old3
w<-web2

#this is not row by row
dupes <- which(w$day %in% o$Day &
                 w$month %in% o$Month &
                 w$year %in% o$Year & 
                 w$species_code %in% o$Species)


#m2 is the smaller one. 2 and 4 
w<-m2
o<-m1
dupes=c()
for(i in c(1:nrow(w))){
  for (j in c(1:nrow(o))){
    if  (w$day[i] %in% o$Day[j] &
         w$month[i] %in% o$Month[j] &
         w$year[i] %in% o$Year[j] & 
         w$species_code[i] %in% o$Species[j]){
      dupes <- c(dupes, i)
  }
  }
}
dupes

#first 10 values in dupes in web
w[c(unique(dupes)[1:10]), ]
#All the duplicate web values in one big table + row number
w_dupes <- w[ c(unique(dupes)),]
w_dupes<- w_dupes %>% mutate(row_number = unique(dupes))
write.csv(w_dupes, "w_dupes_in_o.csv") #write csv

#to check one specific row
test_row<-2
i=unique(dupes)[test_row]
match=c()
for (j in c(1:nrow(o))){
  if  (w$day[i] %in% o$Day[j] &
       w$month[i] %in% o$Month[j] &
       w$year[i] %in% o$Year[j] & 
       w$species_code[i] %in% o$Species[j]){
    match <- c(match, j)
  }
}
#So how many things does ith entry in dupes match old?
match_table <- o[c(unique(match)), ]
match_table <- match_table %>% mutate(row_num = unique(match))
match_table
write.csv(match_table, "ith_macthes_in_old.csv") #Save this as csv

#compare original entries
c1 <- web[ c(unique(dupes)[test_row]) , ] #from web
c2 <- old[ c(unique(match)+1) , ] #from old
write.csv(c1, "w_comparing.csv")
write.csv(c2, "o_comparing.csv")


#problems:
##dupes contains duplicates just because if there was nothing to add it was repeated
###not sure why?
##FOR loops take too long, there shouldn't be any need to have to go through 100k lines every time
##make filters!! filter by year! then month.
###can maybe do the time thing manually?
##Use an id code next time to identify rows in w
##Also, I want which row of old it matches to just to double check



write.csv(unique(dupes), "dupes.csv")

##test
m1 <-read_csv("m1.csv")
m2 <-read_csv("m2.csv")

ca <-which(m1$name %in% m2$name &
             m1$day %in% m2$day &
             m1$month %in% m2$month &
             m1$year %in% m2$year &
             m1$spotted %in% m2$spotted)
m1$`#`[ca]

