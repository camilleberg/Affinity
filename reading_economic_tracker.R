# this script is specifically for Economic Indicators - Cosnumer Spending 


library(tidyverse)
library(dplyr)
library(lubridate)

# removing extraneous variables and setting proper wd
rm(list = ls())
today <- format(Sys.Date(), "%Y.%m.%d")

read_affinity_tables <- function(location_type) {
  
  dat <- read.csv(paste0("data/Affinity - ", paste0(location_type), " - Daily.csv"))
  
  if(location_type == "County") {
    dat <- dat %>%
      filter(countyfips == 25025) 
    # this is the county fips for Suffolk kCounty 
  } else if (location_type == "State") {
    dat <- dat %>%
      filter(statefips == 25)
    # this is the state fips for MA
  }
  
  # clenaing out unwanted data
  dat  <- dat %>% 
    mutate(Date = paste0(month, "/", day, "/", year)) %>% 
    mutate(week = NA) %>%
    select(week, Date, spend_all) 
  
  # looping through for week values
  if(location_type == "County") {
    dat$week[1] <- 0
    week_num <- 1
    for(i in 2:nrow(dat)) {
      if((i-1) %% 7 == 0) {
        week_num <- week_num + 1
        dat$week[i] <- week_num
      } else {
        dat$week[i] <- week_num
      }
    }
  }
  
  return(dat)
}

# creating one DF

df <- left_join(read_affinity_tables("County"), 
                left_join(read_affinity_tables("State"), read_affinity_tables("National"), by = "Date"), by = "Date") %>%
  select(week, Date, spend_all, spend_all.x, spend_all.y) 

colnames(df) <- c("Week", "Day", "Suffolk", "MA", "US")

# writing and opening the fiel in excel 
write.csv(df, paste0("output/consumer_spending.csv", today))

