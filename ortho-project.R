library(readxl)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(splitstackshape)
library(eply)

setwd("~/GitHub/api222-finalproj/api222-finalproj")
orthoMD510k2020 <- read_excel("orthoMD510k2020.xlsx")

json_enf <- "device-enforcement.json"
enforcement <- jsonlite::fromJSON(txt = json_enf) %>% 
  as.data.frame()

json_510k <- "device-510k.json"
registrations <- jsonlite::fromJSON(txt = json_510k) %>% 
  as.data.frame()

json_recall <- "device-recall.json"
recall <- fromJSON(txt = json_recall) %>% 
  as.data.frame()

recalls <- recall %>% 
  filter(.$results.k_numbers != 'NULL') %>% 
  filter(year(.$results.event_date_initiated) > 2019) %>% 
  mutate(results.k_numbers = as.character(results.k_numbers)) %>% 
  cSplit(., "results.k_numbers", sep = ",", direction = "long") %>% 
  mutate(results.k_numbers = str_remove_all(results.k_numbers,"[c\\(\\)]")) %>% 
  mutate(results.k_numbers = unquote(results.k_numbers)) %>% 
  apply(.,2,as.character) %>% 
  as.data.frame()

leftrecalljoin <- left_join(orthoMD510k2020,recalls,
                        by = c("knumber" = "results.k_numbers"))

innerrecalljoin <- inner_join(orthoMD510k2020,recalls,
                              by = c("knumber" = "results.k_numbers"))

write.csv(recalls,"recalls.csv")
write.csv(leftrecalljoin,"recall_join.csv")
write.csv(innerrecalljoin,"recall_inner.csv")

##Other datasets

json_class <- "device-classification.json"
classifications <- fromJSON(txt = json_class) %>% 
  as.data.frame()

json_pma <- "device-pma.json"
pma <- fromJSON(txt = json_pma) %>% 
  as.data.frame()

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
