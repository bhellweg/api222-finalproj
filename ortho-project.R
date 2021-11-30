library(readxl)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(splitstackshape)
library(eply)
library(glmnet)
library(ggplot2)

setwd("~/GitHub/api222-finalproj/api222-finalproj")

count_na <- function(x) sum(!is.na(x))

orthoMD510k2020 <- read_excel("orthoMD510k2020.xlsx") %>% 
  mutate(year = 2020)
orthoMD510k2019 <- read_excel("orthoMD510k2019.xlsx") %>% 
  mutate(year = 2019)
orthoMD510k2018 <- read_excel("orthoMD510k2018.xlsx") %>% 
  mutate(year = 2018)

orthoMD510 <- rbind(orthoMD510k2020,
                    orthoMD510k2019,
                    orthoMD510k2018)

json_recall <- "device-recall.json"
recall <- fromJSON(txt = json_recall) %>% 
  as.data.frame()

recalls <- recall %>% 
  filter(.$results.k_numbers != 'NULL') %>% 
  filter(year(.$results.event_date_initiated) > 2017) %>% 
  filter(meta.disclaimer == "Do not rely on openFDA to make decisions regarding medical care. While we make every effort to ensure that data is accurate, you should assume all results are unvalidated. We may limit or otherwise restrict your access to the API in line with our Terms of Service.") %>% 
  mutate(results.k_numbers = as.character(results.k_numbers)) %>% 
  cSplit(., "results.k_numbers", sep = ",", direction = "long") %>% 
  mutate(results.k_numbers = str_remove_all(results.k_numbers,"[c\\(\\)]")) %>% 
  mutate(results.k_numbers = unquote(results.k_numbers)) %>% 
  apply(.,2,as.character) %>% 
  as.data.frame() %>% 
  distinct()

leftrecalljoin <- left_join(orthoMD510,recalls,
                        by = c("device" = "results.k_numbers"))%>% 
  mutate(recalled = as.numeric(ifelse(is.na(meta.disclaimer),0,1))) %>% 
  mutate(numpred = apply(.[2:92],1,count_na)) 

innerrecalljoin <- inner_join(orthoMD510,recalls,
                              by = c("device" = "results.k_numbers"))%>% 
  mutate(recalled = as.numeric(ifelse(is.na(meta.disclaimer),0,1))) %>% 
  mutate(numpred = apply(.[2:92],1,count_na)) 

write.csv(recalls,"recalls.csv")
write.csv(leftrecalljoin,"recall_join.csv")
write.csv(innerrecalljoin,"recall_inner.csv")

x <- leftrecalljoin$numpred
sqrtx <- sqrt(leftrecalljoin$numpred)
y <- leftrecalljoin$recalled

summary(glm(y~x,family = binomial()))
summary(glm(y~sqrtx,family = binomial()))

plot(glm(y~sqrtx))

linecolors <- c("#714C02")
fillcolors <- c("#9D6C06")

ggplot(as.data.frame(x,y),
       aes(x,y))+
  geom_point(position=position_jitter(h=0.1, w=0.1),
             shape = 21, alpha = 0.5, size = 2, color = 'darkblue')+
  xlab('Number of Predicates')+
  ylab('Binary Recall Variable')+
  labs(title = 'Distribution of Predicates and Recall Frequency',
       subtitle = 'Produced for API-222') +
  theme_bw()

recalled <- leftrecalljoin %>% 
  filter(.$recalled == 0)
notrecalled <- leftrecalljoin %>% 
  filter(.$recalled == 1)

mean(recalled$numpred)
mean(notrecalled$numpred)







##Other datasets


json_enf <- "device-enforcement.json"
enforcement <- jsonlite::fromJSON(txt = json_enf) %>% 
  as.data.frame()

json_510k <- "device-510k.json"
registrations <- jsonlite::fromJSON(txt = json_510k) %>% 
  as.data.frame()
registrations2020 <- registrations %>% 
  filter(year(.$results.decision_date) == 2020)

pmarecalls <- recall %>% 
  filter(.$results.pma_numbers != "NULL" & .$results.pma_numbers != "N/A") %>% 
  filter(year(.$results.event_date_initiated) == 2020) %>% 
  mutate(results.pma_numbers = as.character(results.pma_numbers)) %>% 
  cSplit(., "results.k_numbers", sep = ",", direction = "long") %>% 
  mutate(results.pma_numbers = str_remove_all(results.pma_numbers,"[c\\(\\)]")) %>% 
  mutate(results.pma_numbers = unquote(results.pma_numbers)) %>% 
  #apply(.,2,as.character) %>% 
  as.data.frame()

json_class <- "device-classification.json"
classifications <- fromJSON(txt = json_class) %>% 
  as.data.frame()

json_pma <- "device-pma.json"
pma <- fromJSON(txt = json_pma) %>% 
  as.data.frame()
pma2020 <- pma %>% 
  filter(year(.$results.decision_date) > 2019)
  

json_event01 <- "device-event-01.json"
json_event02 <- "device-event-02.json"
json_event03 <- "device-event-03.json"
json_event04 <- "device-event-04.json"
json_event05 <- "device-event-05.json"
json_event06 <- "device-event-06.json"
json_event07 <- "device-event-07.json"
json_event08 <- "device-event-08.json"
json_event09 <- "device-event-09.json"
json_event10 <- "device-event-10.json"
json_event11 <- "device-event-11.json"
json_event12 <- "device-event-12.json"
json_event13 <- "device-event-13.json"
json_event14 <- "device-event-14.json"
json_event15 <- "device-event-15.json"
json_event16 <- "device-event-16.json"
json_event17 <- "device-event-17.json"

event01 <- fromJSON(txt = json_event01) %>% as.data.frame()
event02 <- fromJSON(txt = json_event02) %>% as.data.frame()
event03 <- fromJSON(txt = json_event03) %>% as.data.frame()
event04 <- fromJSON(txt = json_event04) %>% as.data.frame()
event05 <- fromJSON(txt = json_event05) %>% as.data.frame()
event06 <- fromJSON(txt = json_event06) %>% as.data.frame()
event07 <- fromJSON(txt = json_event07) %>% as.data.frame()
event08 <- fromJSON(txt = json_event08) %>% as.data.frame()
event09 <- fromJSON(txt = json_event09) %>% as.data.frame()
event10 <- fromJSON(txt = json_event10) %>% as.data.frame()
event11 <- fromJSON(txt = json_event11) %>% as.data.frame()
event12 <- fromJSON(txt = json_event12) %>% as.data.frame()
event13 <- fromJSON(txt = json_event13) %>% as.data.frame()
event14 <- fromJSON(txt = json_event14) %>% as.data.frame()
event15 <- fromJSON(txt = json_event15) %>% as.data.frame()
event16 <- fromJSON(txt = json_event16) %>% as.data.frame()
event17 <- fromJSON(txt = json_event17) %>% as.data.frame()

events <- rbind(event01,event02,event03,event04,event05,
                event06,event07,event08,event09,event10,
                event11,event12,event13)
events <- events%>% 
  apply(.,2,as.character) %>% 
  as.data.frame()
write.csv(events,"events.csv")
