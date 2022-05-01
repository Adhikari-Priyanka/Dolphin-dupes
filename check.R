
#is this in line with the manual check?
mn_dupe <- which(web_og$ï..DUP== "yes")
length(mn_dupe)
length(which(mn_dupe %in% time_result$Web_ids_of_dupes))



#checking final_result in r
i=floor(runif(1, 1, nrow(final_result)))
i=which(final_result==883)
web[ final_result[i,1], 8:13]
old[ final_result[i, 2], 9:14]

