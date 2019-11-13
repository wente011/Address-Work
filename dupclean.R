library(readxl)
library(lubridate)
library(tidyverse)
library(magrittr)
library(stringr)
library(stringdist)
library(fuzzyjoin)

dups<-read_excel("addresses_waste.xlsx",sheet="dups")

#dups2<-aggregate(data=dups,)

dups2<- dups %>% group_by(dupcheck) %>% summarize_if(is.numeric,sum,na.rm=T)

                  



sum(na.omit(dups2$`Trash (lbs)`))

sum(na.omit(dups$`Trash (lbs)`))   #73614.06 -- that should be the trash total. 

write.csv(dups2,"dups2.csv")

