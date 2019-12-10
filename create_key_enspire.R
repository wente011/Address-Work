######### USE THIS SCRIPT TO MATCH UP ADDRESSSES AND TO CREATE A "KEY DATASET"

#This code is written explicitly for matching the waste management data from ENSPIRE to the SRT data (by getting to the site ID). A similar script will exisit for the ReTrac downloads, too.  

library(readxl)
library(lubridate)
library(tidyverse)
library(magrittr)
#devtools::install_github("moodymudskipper/safejoin")
library(stringr)
library(stringi)
library(stringdist)
library(ggmap)
library(shiny)
library(editData)
library(openxlsx)
library(fuzzyjoin)
library(safejoin)
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





########################################### Cleaning and Validating #########################################################

rtr<-read_excel("DPS2019_retrac.xlsx")
b3a<-read_excel("all_addresses.xlsx")
ens<-read_csv("waste2.csv") %>% mutate(Zip2=stri_split_fixed(ens$ZIP,"-",simplify = T)[,1]) %>% mutate(IDs=seq(1:length(`Loc ID`)))
srt<-read_excel("solidwastetemplate2019.xlsx")
srt %<>% mutate_at(c("From Date","To Date"),as.Date)


b3a$City[grepl(c("St. Paul"),b3a$City)]<-"Saint Paul"
b3a$City[grepl(c("St Paul"),b3a$City)]<-"Saint Paul"
b3a$City[grepl(c("ST. PAUL"),b3a$City)]<-"Saint Paul"

ensads<-levels(as.factor(ens$`Service Address`)) %>% tibble()
names(ensads)<-c("Address")
ensads$City<-ens$City[match(ensads$Address,ens$`Service Address`)]
ensads$IDs<-seq(1,length(ensads$City))
ensads %<>% mutate(State="MN",IDs=seq(1,length(ensads$City)),Address2=paste0(Address,", ",City,", ",State))

test<-list()
for(i in 1:length(ensads$IDs)){
  test[[i]]<-validate_address_usps(username='230STATE3781',city=ensads$City[i],street=ensads$Address[i],state="MN")
  test[[i]]$IDs<-i
  test[[i]]<-test[[i]][1,]
  
}

test<-bind_rows(test, .id = "column_label") %>% mutate_all(unlist)
test2<-left_join(test,ensads,by="IDs")
test2$City.x<-ifelse(is.na(test2$City.x),test2$City.y,test2$City.x)
test2$Address2.x<-ifelse(is.na(test2$Address2.x),test2$Address,test2$Address2.x)
ensads %<>% select(`Service Address`="Address",IDs)
ensads2<-test2 %>% mutate(Address=paste0(Address2.x,", ",City.x,", ","MN")) %>% select(Address,IDs) %>% left_join(.,ensads)
ensads2$`Location Name`<-ens$`Location Name`[match(ensads2$`Service Address`,ens$`Service Address`)]

b3b<-b3a %>% mutate_at("Address",add_clean) #For use in the results.xlsx file
b3a<- b3a %>% mutate(Address2=Address) %>% mutate(Address=paste0(Address,", ",City,", ","mn")) %>% mutate_at("Address",add_clean) %>% mutate_at(c("City","Address2"),add_clean) %>% select(`Organization Name`,`Address`,`Site ID`,`Site Name`)
ensads2$Address %<>% add_clean()   #There are 57 distinct service addresses currently in the database.


########################################### Joining (EDIT WITH NEW SITES) #####################################################################

results<-addJoin(x=ensads2,b3=b3a,threshold=0.20) %>% arrange(distance) %>% mutate_at(c("Address.x","Address.y","Service Address","Site Name"),as.factor)
chck<-results %>% group_by(`IDs`,`Site Name`,`Address.x`,`Address.y`,`Service Address`,`Site ID`,`Location Name`,`Organization Name`) %>% summarize_at("distance",min) %>% arrange(distance)
chck<-chck[!duplicated(chck$`Service Address`),]
chck2<-results[results$IDs %!in% chck$IDs & !duplicated(results$`Service Address`),] %>% bind_rows(.,chck) %>% filter(`Location Name`!="City Of New Hope-Public Works")
write.csv(chck2,"qaqc.csv")

#Ok, this seems to work pretty well! It grabs the data pretty well. The next function opens a shiny app to direct enter the SITE IDs. 
chck2[is.na(chck2$`Site ID`),]<-editData(data=chck2[is.na(chck2$`Site ID`),])



