library(readxl)
library(lubridate)
library(tidyverse)
library(magrittr)
library(stringr)
library(stringdist)
library(fuzzyjoin)

rtr<-read_excel("addresses_waste.xlsx",sheet="wasteaddress")
b3a<-read_excel("addresses_waste.xlsx",sheet="b3address")

b3a$Address<- paste0(b3a$Address,", ",b3a$City,", ",b3a$State," ",b3a$Zip)



# using the jaro-winkler string distance
# but only take a subset of the data, just to speed things up
doTheJoin <- function (threshold) {
  joined <-  rtr %>% 
    stringdist_full_join(
        b3a, 
      by = "Address",
      distance_col = "distance",
      max_dist = threshold,
      method = "jw"
    )
}


results<-doTheJoin(.26)

write.csv(results,"results.csv")

