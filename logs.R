library(benford.analysis)
library(ggthemes)
library(Rmpfr)
library(scales)
library(tidyverse)


limf<-function(n){
 x<-(1 +1/n)^n 
return(x)
}


idx<-seq(0,1000,1)

res<-formatDec(limf(idx),precBits=53)%>% enframe() %>% bind_cols(tibble(idx)) %>% rename(e.approx="value",n="idx")%>% ungroup()
res %>% ggplot(aes(x=n,y=e.approx)) + geom_point() 
                                                        
res$e.str<-res$e.approx %>% as.character()

#bfd.cp<-benford(res$e.approx[20:length(res$e.approx)],number.of.digits = 5)



digs<-res %>% mutate(`0`=str_count(e.str,"0")) %>% mutate(`1`=str_count(e.str,"1")) %>% mutate(`2`=str_count(e.str,"2")) %>% 
  mutate(`3`=str_count(e.str,"3")) %>% mutate(`4`=str_count(e.str,"4")) %>% mutate(`5`=str_count(e.str,"5")) %>% mutate(`6`=str_count(e.str,"6")) %>%
  mutate(`7`=str_count(e.str,"7")) %>% mutate(`8`=str_count(e.str,"8")) %>% mutate(`9`=str_count(e.str,"9")) %>% select(everything(),-name,-e.str) %>% 
      gather(digits,value,-e.approx,-n)

for( i in digs$n){
 ggs<- digs[digs$n==i,] %>%  ggplot(aes(x=digits,y=value)) + geom_bar(stat="identity") + ggtitle(paste0("Distribution of digits in x=(1 +1/n)^n for n=",i)) + xlab("Digits")  + ylim(0,25) + ylab("Count")
 ggsave(filename=paste0("n","_",i,".png"),path="C:/Users/jwente/Downloads/digits in e",device="png") 
}


digs %>% group_by(digits) %>% summarize_at("value",sum) %>% ggplot(aes(x=digits,y=value)) + geom_bar(stat="identity") + ggtitle("Total counts of all calculations")

