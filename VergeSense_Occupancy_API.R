library(httr)
library(jsonlite)
library(lubridate)
library(plyr)
library(dplyr)
library(readr)
library(pander)
library(taskscheduleR)
library(miniUI)
library(knitr)
library(shiny)
library(data.table)
library(sendmailR)

#API for gathering occupied desks and sending an end of day email for cleaning.
#Create a data frame for capturing the data
count <- 1
minutes_occupied <- 10
location <- "Test"
Final <- data.frame(location, count, minutes_occupied)

#Populate a list with all of the current spaces
response <- GET("https://api.vergesense.com/spaces",
                add_headers("vs-api-key" = "API_Code_Provided_By_Vergesense"))
data <- fromJSON(rawToChar(response$content))
data$space_ref_id <- gsub(" ", "_", data$space_ref_id)
SpaceID <- data$space_ref_id

#Get date and time range
Date <- as_date(paste(format_ISO8601(Sys.time(), tz = "UTC"),"Z", sep = ""), tz = NULL)
DateStart <- paste(Date, "T12:00:00Z", sep = "")
DateStop <- paste(Date, "T23:59:59Z", sep = "")

#Get information about the days occupied spaces
for(i in 1:length(SpaceID)){
  URL <- paste("https://api.vergesense.com/buildings/Your_Building/history?start_timestamp=",
               DateStart,"&end_timestamp=",DateStop,"&space_ref_id=",SpaceID[[i]], sep = "")
  response <- GET(URL,add_headers("vs-api-key" = "API_Code_Provided_By_Vergesense"))
  data <- fromJSON(rawToChar(response$content))
  data2 <- data$history
  data2 <- as.data.frame(data2)
  if (empty(data2) == "TRUE") {
    print(paste(SpaceID[[i]],"had no occupancy"))
  } else if 
  (length(data2$count) < 2) {
    print(paste(SpaceID[[i]],"had no occupancy"))
  } else if 
  (max(data2$count) < 1) {
    print(paste(SpaceID[[i]],"had no occupancy"))
  } else {
    data2$signs_of_life <-NULL 
    Cleaning <- data2
    Cleaning$location <- SpaceID[[i]]
    Cleaning <- Cleaning[Cleaning$count != 0,]
    maxCount <- max(as.numeric(Cleaning$count), na.rm = FALSE)
    Cleaning$count <- maxCount
    Cleaning$timestamp <- nrow(Cleaning)*10
    Cleaning$minutes_occupied <- Cleaning$timestamp
    Cleaning$timestamp <- NULL
    Cleaning <- Cleaning[1,]
    Cleaning <- Cleaning[,c(2,1,3)]
    Final <- bind_rows(Final, Cleaning)
  }
}
#Prepare email for distribution
#Remove the first row of Final since this was only used to prepare the data frame
Final <- Final[-1,]
if (nrow(Final) <1) {
  from <- sprintf("<shyland@company.com>","The Sender") # the sender's name is an optional value
  to <- sprintf(c("person@company.com","person@company.com","person@company.com", "person@company.com"))
  subject <- "No spaces needing to be cleaned today"
  body <- "There are no spaces in C2 that need to be cleaned today."
  sendmail(from,to,subject,body,control=list(smtpServer= "smtp.company.com"))
} else if 
(nrow(Final) <2) {
  Finals <- as.list(Final)
  Finals <- paste( unlist(Finals[[1]]), collapse=', ')
  Finals <- paste("The following space needs to be cleaned as of ", Date, ":", Finals, sep = " ")
  from <- sprintf("<shyland@company.com>","The Sender") # the sender's name is an optional value
  to <- sprintf(c("person@company.com","person@company.com","person@company.com", "person@company.com"))  sprintf(c("<shyland@amfam.com>"))
  subject <- "Daily notifications of what spaces need to be cleaned as part of NEXT"
  body <- Finals
  sendmail(from,to,subject,body,control=list(smtpServer= "smtp.company.com"))
} else {
  Finals <- as.list(Final)
  Finals <- paste( unlist(Finals[[1]]), collapse=', ')
  Finals <- paste("The following spaces need to be cleaned as of ", Date, ":", Finals, sep = " ")
  from <- sprintf("<shyland@company.com>","The Sender") # the sender's name is an optional value
  to <- sprintf(c("person@company.com","person@company.com","person@company.com", "person@company.com"))  subject <- "Daily notifications of what spaces need to be cleaned as part of NEXT"
  body <- Finals
  sendmail(from,to,subject,body,control=list(smtpServer= "smtp.company.com"))
}