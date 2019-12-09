library(readxl)
library(lubridate)
library(tidyverse)
library(magrittr)
library(stringr)
library(stringi)
library(stringdist)
library(fuzzyjoin)

#devtools::install_github("hansthompson/rusps")

library(rusps)
library(XML)





b3a<-read_excel("all_addresses.xlsx")
rtr<-read_excel("DPS2019_retrac.xlsx")


b3a$City[grepl(c("St. Paul"),b3a$City)]<-"Saint Paul"
b3a$City[grepl(c("St Paul"),b3a$City)]<-"Saint Paul"
b3a$City[grepl(c("ST. PAUL"),b3a$City)]<-"Saint Paul"

rtr$`City:`[grepl(c("St. Paul"),rtr$`City:`)]<-"Saint Paul"
rtr$`City:`[grepl(c("St Paul"),rtr$`City:`)]<-"Saint Paul"
rtr$`City:`[grepl(c("ST. PAUL"),rtr$`City:`)]<-"Saint Paul"

b3a<- b3a %>% mutate(Address=paste0(Address,", ",City,", ",Zip)) %>% mutate_at("Address",add_clean) %>% select(Address,`Site Name`,`Site ID`)

rtr %<>% mutate(Address=paste0(`Street Address:`,", ",`City:`,", ",`Zip Code:`)) %>% mutate_at("Address",add_clean)



test<-doTheJoin(rtr,b3a,0.11)
write.csv(test)