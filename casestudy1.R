#install packages
install.packages('tidyverse')
install.packages("janitor") 
install.packages("lubridate")
install.packages("devtools")
install.packages("psych")
install.packages("hunspell")
install.packages('https://cran.r-project.org/src/contrib/Archive/hrbrthemes/hrbrthemes_0.1.0.tar.gz', type='source', repos=NULL)

#load packages

library(tidyverse)
library(lubridate)
library(janitor)
library(data.table)
library(readr)
library(psych)
library(hrbrthemes)
library(ggplot2)
#collecting data
december_2020 <- read.csv("202012-divvy-tripdata.csv")
january_2021 <- read.csv("202101-divvy-tripdata.csv")
february_2021 <- read.csv("202102-divvy-tripdata.csv")
march_2021 <- read.csv("202103-divvy-tripdata.csv")
april_2021 <- read.csv("202104-divvy-tripdata.csv")
may_2021 <- read.csv("202105-divvy-tripdata.csv") 
june_2021 <- read.csv("202106-divvy-tripdata.csv")
july_2021 <- read.csv("202107-divvy-tripdata.csv")
august_2021 <- read.csv("202108-divvy-tripdata.csv")
september_2021 <- read.csv("202109-divvy-tripdata.csv")
october_2021 <- read.csv("202110-divvy-tripdata.csv")
november_2021 <- read.csv("202111-divvy-tripdata.csv")


#data validation to check if column names are the same in all the sheets
colnames(december_2020)
colnames(january_2021)
colnames(february_2021)
colnames(march_2021)
colnames(april_2021)
colnames(may_2021)
colnames(june_2021)
colnames(july_2021)
colnames(august_2021)
colnames(september_2021)
colnames(october_2021)
colnames(november_2021)
#data validation to calculate the number of rows in all the sheets

sum(nrow(december_2020) + nrow(january_2021) + nrow(february_2021) 
    + nrow(march_2021) + nrow(april_2021) + nrow(may_2021) 
    + nrow(june_2021) + nrow(july_2021) + nrow(august_2021)
    + nrow(september_2021) + nrow(october_2021) + nrow(november_2021))
#aggregating all the data into one data frame
trip_final <- rbind(december_2020,january_2021,february_2021,march_2021,april_2021,
                    may_2021,june_2021,july_2021,august_2021,september_2021,october_2021,november_2021)

#saving this file into the system
write.csv(trip_final,file = "trip_final.csv",row.names = FALSE)
#uploading again

trip_final <- read_csv("trip_final.csv")
options(future.globals.maxSize = Inf)

#data validation
str(trip_final)
view(head(trip_final))
view(tail(trip_final))
dim(trip_final)
summary(trip_final)
names(trip_final)

#DAtA CLEANING
  #check null values
colSums(is.na(trip_final))
#remove null
trip_clean_final <- trip_final[complete.cases(trip_final), ]
#remove duplicate
trip_clean_final <- distinct(trip_clean_final)
#check if starting at is > ended at if so then remove
trip_clean_final <- trip_clean_final %>% 
  filter(trip_clean_final$started_at < trip_clean_final$ended_at)
#rename some fields for better understanding
trip_clean_final <- rename(trip_clean_final, customer_type = member_casual,bike_type = rideable_type)
trip_clean_final <- rename(trip_clean_final, start_time = started_at)
#data cleaning
drop_na(trip_clean_final)
remove_empty(trip_clean_final)
remove_missing(trip_clean_final)


#Create additional columns for Date, Month, Day, Year, day of the Week from the started_at column.This allows for more granular analysis of the data by date/day/month.->
  
  trip_clean_final$date <- as.Date(trip_clean_final$start_time)
trip_clean_final$week_day <- format(as.Date(trip_clean_final$date), "%A")
trip_clean_final$month <- format(as.Date(trip_clean_final$date), "%b_%y")
trip_clean_final$year<-format(trip_clean_final$date,"%Y")
#Add a new column named time.
  
 trip_clean_final$time <- format(trip_clean_final$start_time, format = "%H:%M")
 
#Change format for the time column for later use.->
  trip_clean_final$time <- as.POSIXct(trip_clean_final$time, format = "%H:%M") 
  
#Create a column for duration of rides calculated from start and end time of rides.->
  trip_clean_final$ride_length <- difftime(trip_clean_final$ended_at,trip_clean_final$start_time, units = "mins")
  
#Filter out data that we will not be using for this analysis.->
  trip_clean_final <- trip_clean_final %>% 
  select(bike_type, customer_type, month, year, time, start_time, week_day, ride_length)
  
#Get rid of too long rides as rides should be limited to 1 day or 1440 minutes or 24Hr(cyclistic considers these bikes are stolen).->
  trip_clean_final <- trip_clean_final[!trip_clean_final$ride_length>1440,] 
  
#Get rid of negative rides.->
  trip_clean_final <- trip_clean_final[!trip_clean_final$ride_length<5,] 

# save the final file
  write.csv(trip_clean_final,file = "trip_clean_final.csv",row.names = FALSE)
#upload it again to check
  trip_clean_final <- read_csv("trip_clean_final.csv")
  options(future.globals.maxSize = Inf)
  #data validation
  str(trip_clean_final)
  names(trip_clean_final)
  
 # Sorting Month and week days in order.->
    
  #  Sort the week days and month in order for future analysis process.
  trip_clean_final$month <- ordered(trip_clean_final$month,
                                    levels=c("Dec_20", "Jan_21", "Feb_21", "Mar_21", 
                                             "Apr_21", "May_21", "Jun_21", "Jul_21", 
                                             "Aug_21", "Sep_21", "Oct_21", "Nov_21"))
  
  trip_clean_final$week_day <- ordered(trip_clean_final$week_day, 
                                       levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", 
                                                "Friday", "Saturday"))
  #min max median sd of ride length
  view(describe(trip_clean_final$ride_length, fast=TRUE)) 
  view(table(trip_clean_final$customer_type))
  view(setNames(aggregate(ride_length ~ customer_type, trip_clean_final, sum), 
                c("customer_type", " total_ride_length(mins)")))
  view(trip_clean_final %>% 
         group_by(customer_type) %>% 
         summarise(min_length_minutes = min(ride_length), max_length_minutes = max(ride_length), 
                   median_length_minutes = median(ride_length), mean_length_minutes = mean(ride_length)))
  
  view(trip_clean_final %>% 
         group_by(week_day) %>% 
         summarize(Avg_length = mean(ride_length),
                   number_of_rides = n(),))
  #Analysis-6: Number of average rides by month.->
    
    view(trip_clean_final %>% 
           group_by(month) %>% 
           summarize(Avg_length = mean(ride_length),
                     number_of_rides = n())
    )
 # Analysis-7:Average ride length comparison by each week day according to each customer type.->
    
    view(aggregate(trip_clean_final$ride_length ~ trip_clean_final$customer_type + 
                     trip_clean_final$week_day, FUN = mean))
 # Analysis-8:Average ride length comparison by each month according to each customer type.->
    
    view(aggregate(trip_clean_final$ride_length ~ trip_clean_final$customer_type + 
                     trip_clean_final$month, FUN = mean))
 ## Analysis-9:Analyze rider length data by customer type and weekday.->
    
    view(
      trip_clean_final %>% 
        group_by(customer_type, week_day) %>% 
        summarize(number_of_rides = n(),
                  average_duration = mean(ride_length),
                  median_duration = median(ride_length),
                  max_duration = max(ride_length),
                  min_duration = min(ride_length)) 
    )
 # Analysis-10:Analyze rider length data by customer type and month.->
    
    view(
      trip_clean_final %>% 
        group_by(customer_type, month) %>% 
        summarize(number_of_rides = n(),
                  average_duration = mean(ride_length),
                  median_duration = median(ride_length),
                  max_duration = max(ride_length),
                  min_duration = min(ride_length)) 
    )
   
    write.csv(trip_clean_final,file = "trip_clean_final_tableau.csv",row.names = FALSE)
    