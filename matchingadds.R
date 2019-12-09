############################  Intro:
#This sheet is for converting W&M to SRT compatible data. The functions can be used for converting Re-Trac as well. 

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


#Wrap the nice USPS validation algorithm:
usps.val<-function(address,city,state,ID){
   username <- '230STATE3781'   #USPS username 
    x<- validate_address_usps(username=username,address,city,state)   #need to load this function using the devtool rusps package.
    x[,7]<-ID
}
  




########################################### Data Load (add files as needed) #####################################################################
rtr<-read_excel("addresses_waste.xlsx",sheet="wasteaddress")
b3a<-read_excel("all_addresses.xlsx")
ens<-read_csv("waste2.csv") %>% mutate(Zip2=stri_split_fixed(ens$ZIP,"-",simplify = T)[,1]) %>% mutate(IDs=seq(1:length(`Loc ID`)))
srt<-read_excel("solidwastetemplate2019.xlsx")
ens.key<-read.csv("ens.key.csv") %>% select(`Serivce Address`="Service.Address",`Site ID`="Site.ID")
ens.keyed<-ens
ens.keyed$`Site ID`<-ens.key$`Site ID`[match(ens$`Service Address`,ens.key$`Serivce Address`)]

########################################### Check if there are new sites #####################################################################
ens.keyed[is.na(ens.keyed$`Site ID`),]  #OK, just city of new hope. 


########################################### Cleaning and Validating #########################################################
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
  test[[i]]<-validate_address_usps(username=username,city=ensads$City[i],street=ensads$Address[i],state="MN")
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
#ens.key<-chck2 %>% select(`Service Address`,`Site ID`), 
#write.csv(ens_key,"ens.key.csv")    #this is to create a file 



####################################### RESULTS USING THE KEYED DATASET #############################################################

#Mutate the data now; make SRT compatible template!
#You're going to keep making those templates either way. 

ens.keyed %<>% mutate_at("Service Date",mdy) %>% mutate(`To Date`= ceiling_date(`Service Date`,"month")-days(1)) %>% mutate_at(c("Material Category","Size"),as.factor)
  
ens.keyed$`Material Category` %<>% as.factor() %>% fct_recode(.,`Trash (lbs)`="Trash",`Recycling: Single Stream Actual Weight (in Pounds)`="Single-Stream Recycling",`Recycling: Dual stream (Paper, including cardboard) Actual Weight (in Pounds)`="Cardboard",`Recycling: Single Stream Actual Weight (in Pounds)`="Single Stream Recycling",`Construction Material Actual Weight (in Pounds)`="Construction / Demolition")

#Get yards into here:

yds<-cbind.data.frame(levels(ens.keyed$Size),c(0,0.5,1,1.5,2,20,3,30,4,6,8,NA))
names(yds)<-c("Size","size_num")  
ens.keyed2<- ens.keyed %>% left_join(.,yds) %>% select(Address=`Service Address`,`Facility ID`="Site ID",`From Date`="Service Date",`To Date`,size_num,Tonnage,`Material Category`,QTY) %>% mutate(Yards=size_num*QTY) %>%
               mutate(lbs=Tonnage*2000,lbs.yard=lbs/Yards) %>% filter(`Material Category`!="Not Applicable",is.na(`Facility ID`)==F) #yards and tonnage are summed, but not lbs. 

#Add "Member" and change service address to B3, ugly. but it works. 
b3a<-b3a<-read_excel("all_addresses.xlsx")
ens.keyed2$Address<-b3a$Address[match(ens.keyed2$`Facility ID`,b3a$`Site ID`)] 
ens.keyed2$Address[is.na(ens.keyed2$Address)]<-ens.keyed2$`Facility ID`[is.na(ens.keyed2$Address)]

ens.keyed2$Member<-b3a$`Site Name`[match(ens.keyed2$`Facility ID`,b3a$`Site ID`)] 
ens.keyed2$Member[is.na(ens.keyed2$Member)]<-ens.keyed2$`Facility ID`[is.na(ens.keyed2$Member)]
  

ens.keyed2$`Material Category` %<>% droplevels()

cnvs<-cbind.data.frame((levels(ens.keyed2$`Material Category`)),c(150,1,1073,139,350,350))
names(cnvs)<-c("Material Category","Conversion")

ens.keyed2.1<-ens.keyed2 %>% left_join(.,cnvs) %>% mutate(lbs2=Yards*Conversion) 
ens.keyed2.1$`Material Category` %<>% fct_recode(`Organics (lbs)`="Yard Waste",`Organics (lbs)`="Food Waste") %>% droplevels()
ens.keyed2.1$lbs2[ens.keyed2.1$`Material Category`=="Construction Material Actual Weight (in Pounds)"]<-ens.keyed2.1$lbs[ens.keyed2.1$`Material Category`=="Construction Material Actual Weight (in Pounds)"] 
ens.keyed2.1 %<>% rowid_to_column()
ens.keyed2.2<-ens.keyed2.1 %>% spread(.,`Material Category`,lbs2) %>% mutate(`From Date`=dmy(paste0("01","/",month(`From Date`),"/",year(`From Date`)))) %>% group_by(`Facility ID`,`From Date`) %>% mutate_at(c("Conversion","lbs.yard","Tonnage","size_num"),as.factor) %>% summarise_if(is.numeric,sum,na.rm=T)  %>% ungroup %>% mutate(`To Date`= `From Date`)
day(ens.keyed2.2$`To Date`)<-days_in_month(ens.keyed2.2$`From Date`)

#ens.keyed2.2$size_num<-ens.keyed2.1$size_num[match(ens.keyed2.2$rowid,ens.keyed2.1$rowid)]
srt2<-srt %>% mutate_all(as.character) 
srt2<-srt2[c(0),]




ens.keyed3<-bind_rows(ens.keyed2.2,srt2)
ens.keyed3 %>% left_join(ens.keyed2,srt,by="Facility ID")
ens.keyed3[is.na(ens.keyed3)]<-""

#ens.keyed3<-safe_left_join(ens.keyed2.2,by=c("Facility ID"),conflict=coalesce)


openxlsx::write.xlsx(ens.keyed3,"results.xlsx") #works perfectly now. Got perfect matches  


#Visualize the issues with W&M conversions 

results2.1 %>% ggplot(aes(x=lbs.yard)) + geom_histogram() + facet_wrap(~`Material Category`) + ggtitle("Conversion Rates in the W&M dataset")

