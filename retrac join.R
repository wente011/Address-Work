library(readxl)
library(lubridate)
library(tidyverse)
library(magrittr)
library(stringr)
library(stringi)
library(safejoin)
library(stringdist)
library(fuzzyjoin)

#devtools::install_github("hansthompson/rusps")

library(rusps)
library(XML)



############################################ Functions #####################################################################
'%!in%' <- function(x,y)!('%in%'(x,y))  #I always like to create my own "opposite of %in%" for nice logicals.

# using the jaro-winkler string distance, must have an address vector called "Address" 
addJoin <- function(x,b3,threshold){
  joined <- 
    stringdist_left_join(x,
                         b3, 
                         by = "Address",
                         distance_col = "distance",
                         max_dist = threshold,
                         method = "jw"
    )
}


#Need to convert all chars to lower case for easier matching. The column names and order MATTER. Edit the list in the function to add more terms to clean. 
add_clean<-function(char){
  lng<-c("us","-","avenue","street","highway","lane","drive","alley","boulevard","circle","court","county","north","west","east","south","road","second","first","third",".")
  shrt<-c("hwy"," ","ave","st","hwy","ln","dr","aly","blvd","cir","ct","co","n","w","e","s","rd","2nd","1st","3rd","")
  char %<>% stri_trans_tolower()    
  for (i in 1:length(lng)){
    char<-stri_replace_all_fixed(char,lng[i],shrt[i]) }
  char
}




#################################################### Data loading and joining ###########################################################


b3a<-read_excel("all_addresses.xlsx")
rtr<-read_excel("DPS2019_retrac.xlsx")
srt<-read_excel("solidwastetemplate2019.xlsx")
srt2<-srt[c(0),] %>% mutate_all(as.character)

b3a$City[grepl(c("St. Paul"),b3a$City)]<-"Saint Paul"
b3a$City[grepl(c("St Paul"),b3a$City)]<-"Saint Paul"
b3a$City[grepl(c("ST. PAUL"),b3a$City)]<-"Saint Paul"

rtr$`City:`[grepl(c("St. Paul"),rtr$`City:`)]<-"Saint Paul"
rtr$`City:`[grepl(c("St Paul"),rtr$`City:`)]<-"Saint Paul"
rtr$`City:`[grepl(c("ST. PAUL"),rtr$`City:`)]<-"Saint Paul"

b3b<-b3a %>% mutate_at("Address",add_clean)   #for matching back values in the results. 
b3a<- b3a %>% mutate(Address=paste0(Address,", ",City)) %>% mutate_at("Address",add_clean) %>% select(Address,`Site Name`,`Site ID`)

rtr %<>% mutate(Address=paste0(`Street Address:`,", ",`City:`)) %>% mutate_at("Address",add_clean)

#Create vector for checking vals
qaqc<-rtr %>% summarize_at(c("Trash (lbs)","Recycling (lbs)","Organics (lbs)"),sum,na.rm=T) %>% mutate(diversion.rate=(`Organics (lbs)` + `Recycling (lbs)`)/(`Organics (lbs)` + `Recycling (lbs)` + `Trash (lbs)`))
qaqc

rtr2<-addJoin(rtr,b3a,0.07)   #should be a perfect match. 
#rtr2[is.na(rtr2)]<-""


rtr2 %<>% mutate(`From Date`=mdy(`Reporting Date/Period`),`To Date`=`From Date`)
day(rtr2$`To Date`)<-days_in_month(rtr2$`From Date`)
rtr2$`Agency Name`<-b3b$`Agency Name`[mf]
rtr2$`Facility ID`<-rtr2$`Site ID`

################# Recode vars ################################

rtr2 %<>% rename(`Does your agency have short-term visitors (e.g., public meetings, hearings, large events, etc.) who contribute to your waste/recycling generation?`="Does your agency have short-term visitors (e.g., public meetings, hearings, large events, etc.) who contribute to your waste/recycling generation?",
                `Total Staff:`="Staff Population",`Is your agency location following the Green Meeting Policy for internal and external meetings of all sizes?`="The State of Minnesota has established a Green Meeting Policy. This can be provided by the Minnesota Department of Administration and the Minnesota Pollution Control Agency. Is your agency location following the Green Meeting Policy for internal and external meetings of all sizes?") %>%
                mutate(`Total Population`=`Total Staff:`+ `Non-Staff Population`) %>% mutate(`Office supply reuse center`="Is your agency location utilizing the state's recycling best practices?") %>% 
                mutate_all(as.character) %>% mutate(`Does your agency have short-term visitors (e.g., public meetings, hearings, large events, etc.) who contribute to your waste/recycling generation? `=`Does your agency have short-term visitors (e.g., public meetings, hearings, large events, etc.) who contribute to your waste/recycling generation?`) %>%
                mutate(`Staff Population`=`Total Staff:`)
                
rtr2[is.na(rtr2)]<-""               
                
#Check results before printing:
rtr2 %>% summarize_at(c("Trash (lbs)","Recycling (lbs)","Organics (lbs)"),function(x) sum(as.numeric(x),na.rm = T)) %>% mutate(diversion.rate=(`Organics (lbs)` + `Recycling (lbs)`)/(`Organics (lbs)` + `Recycling (lbs)` + `Trash (lbs)`))  
  
rtr2 %>% mutate_at("Recycling (lbs)",function(x) x<-0) %>% openxlsx::write.xlsx(.,"retrac2srtupload.xlsx")





