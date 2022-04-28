class(web5$Time)
class(old5$Time_start)
class(old5$Time_end)

a=web5$Time[1:10]
b=old5$Time_start[1:10]
c=old5$Time_end[1:10]
n <- rep(0,length(a))
a1<- data.frame(a, n)


dam <- read.csv("test3_dupes_and_matches.csv")
dm <- dam[1:19,]
done <-read.csv("test3_DONE.csv")
d <- done %>% mutate(time_match= rep(0, nrow(done)))
d <- d[5:10, 8:13]



nas <- which(is.na(c(web_time, old_time_end, old_time_start)))

c <- paste( c(web_time, old_time_end, old_time_start)[nas] ," is na")







