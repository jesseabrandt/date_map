library(tidyverse)

# Exercise 1 Question 1
#Generate a sequence of dates from January 1, 2015 to December 31, 2025, spaced by
#every two months. Extract the year, quarter, and ISO week number for each date.

#Interpretation: every 2 months starting January 1, 2015. This will not include December 31, 2025,
#The last date within the date range is November 1, 2025.

start <- mdy("01/01/2015") #start date
end <- mdy("12/31/2025") # end date
intrv <- interval(start, end) #interval between start and end date
period <- (as.period(intrv)) #convert to period

months_total <- (12*period@year + period@month) #find number of full months in period
#this is simple arithmetic, but doing it this way will make it possible to do this for any 2 dates

seq(0, months_total, by = 2) #create a sequence (number of months after start date)

date <- map_vec(seq(0, months_total, by = 2), \(x)start + months(x)) #create date vector
year <- map_int(all_dates, ~year(.)) #Create year column


df <- (data.frame(date, year)) %>% #bind those two columns together
  mutate(quarter = quarter(date), iso_week = isoweek(date)) #add the rest of the columns
#was this supposed to all be done with map?


#Exercise 2 Question 2
#Given the following dates, compute the difference in months and weeks 
#between each consecutive pair.

sample_dates <- c("2018-03-15", "2020-07-20", "2023-01-10", "2025-09-05") %>%
  ymd() #convert sample dates from strings to Dates
dates <- sample_dates %>%
  data.frame() %>%
  select(date = ".") %>% #rename column
  mutate(date_diff = (date - lag(date))%>%as.period(), #find difference between dates
         diff_months = date_diff / months(1), diff_weeks = date_diff / weeks(1)) #convert to weeks and months
# interpretation question - is difference in months and weeks referring to 2 differences? 
# or is it referring to 1 difference, expressed in months,weeks? 


#Exercise 3 Question 3
# Using map() and map_dbl(), compute the mean, median, and standard deviation for each
# numeric vector in the following list

num_lists <- list(c(4, 16, 25, 36, 49), c(2.3, 5.7, 8.1, 11.4), c(10, 20, 30, 40, 50))
.f = function(l){
  return(c(mean(l), median(l), sd(l)))
}
map(num_lists, .f)

#why use map and map_dbl? there is only one list which needs functions applied to each item
#the inner lists need multiple functions applied to the whole list instead

#Exercise 4 Question 4
#Given a list of mixed date formats, use map() and possibly() from purrr to safely convert
#them to Date format and extract the month name.

#Assumptions: possible formats are ymd and dmy. (Without assumption, date 1 is ambiguous)

date_strings <- list("2023-06-10", "2022/12/25", "15-Aug-2021", "InvalidDate")
d = date_strings[1]
date_convert = function(d){
  date = ifelse(is.na(ymd(d)), dmy(d), ymd(d)) %>%
    as.Date()
  return(date)
}
possibly_convert <- possibly(date_convert)
possibly_month <- possibly(~month(., label = T))
formatted_dates <- map_vec(date_strings, possibly_convert)
months <- map_vec(formatted_dates, possibly_month)
data.frame(formatted_dates, months)
