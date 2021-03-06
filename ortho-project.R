library(readxl)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(eply)
library(glmnet)
library(ggplot2)
library(jsonlite)
library(splitstackshape)

setwd("~/GitHub/api222-finalproj/api222-finalproj")

## Set up Functions and Datasets

#Function to count the number of NAs in a dataset
count_na <- function(x) sum(!is.na(x))

#Function to sort down to a single K number in the registrations dataset
devyr <- function(knum){
  registrations %>% 
    filter(results.k_number == knum) %>% 
    select(results.decision_date) %>% 
    as.character() 
}

#Function to ensure all dates for recalls are properly formatted
asdate1 <- function(x){
  as.Date(x,origin = origin)
}

#Function to show the mean date for all recalls of predicates by device
meandate1 <- function(x){
  x %>% unlist() %>%  mean.Date(na.rm = T)
}

#Load the relevant ortho datasets
orthoMD510k2020 <- read_excel("orthoMD510k2020.xlsx") %>% 
  mutate(year = 2020)
orthoMD510k2019 <- read_excel("orthoMD510k2019.xlsx") %>% 
  mutate(year = 2019)
orthoMD510k2018 <- read_excel("orthoMD510k2018.xlsx") %>% 
  mutate(year = 2018)

#Merge the datasets
orthoMD510 <- rbind(orthoMD510k2020,
                    orthoMD510k2019,
                    orthoMD510k2018)

#Load and process the json files for recalls
json_recall <- "device-recall.json"
recall <- fromJSON(txt = json_recall) %>% 
  as.data.frame()

#Load and process the 510K dataset
json_510k <- "device-510k.json"
registrations <- jsonlite::fromJSON(txt = json_510k) %>% 
  as.data.frame()

#Produce a dataset identifying recalls issued for all devices, splitting out devices with multiple K numbers into multiple rows
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

#Checking that the function works and produces the right decision date based on the K number
registrations %>% 
  filter(results.k_number == "K112028") %>% 
  select(results.decision_date) %>% 
  as.character() %>% 
  as.Date()
devyr("K112028")

#Producing a new dataset showing the recall date (if applicable) for every device by K number, with each predicate mapped out for recall date.
registrationjoin <- left_join(orthoMD510,registrations,
                               by = c("device" = "results.k_number"))%>% 
  mutate(numpred = apply(.[2:92],MARGIN = 1,FUN = count_na)) %>% 
  select(-c(109,110)) %>% 
  mutate(predyr1 = sapply(predicates1,FUN = devyr)) %>% 
  mutate(predyr2 = sapply(predicates2,FUN = devyr))%>% 
  mutate(predyr3 = sapply(predicates3,FUN = devyr))%>% 
  mutate(predyr4 = sapply(predicates4,FUN = devyr))%>% 
  mutate(predyr5 = sapply(predicates5,FUN = devyr))%>% 
  mutate(predyr6 = sapply(predicates6,FUN = devyr))%>% 
  mutate(predyr7 = sapply(predicates7,FUN = devyr))%>% 
  mutate(predyr8 = sapply(predicates8,FUN = devyr))%>% 
  mutate(predyr9 = sapply(predicates9,FUN = devyr))%>% 
  mutate(predyr10 = sapply(predicates10,FUN = devyr))%>% 
  mutate(predyr11 = sapply(predicates11,FUN = devyr))%>% 
  mutate(predyr12 = sapply(predicates12,FUN = devyr))%>% 
  mutate(predyr13 = sapply(predicates13,FUN = devyr))%>% 
  mutate(predyr14 = sapply(predicates14,FUN = devyr))%>% 
  mutate(predyr15 = sapply(predicates15,FUN = devyr))%>% 
  mutate(predyr16 = sapply(predicates16,FUN = devyr))%>% 
  mutate(predyr17 = sapply(predicates17,FUN = devyr))%>% 
  mutate(predyr18 = sapply(predicates18,FUN = devyr))%>% 
  mutate(predyr19 = sapply(predicates19,FUN = devyr))%>% 
  mutate(predyr20 = sapply(predicates20,FUN = devyr))%>% 
  mutate(predyr21 = sapply(predicates21,FUN = devyr))%>% 
  mutate(predyr22 = sapply(predicates22,FUN = devyr))%>% 
  mutate(predyr23 = sapply(predicates23,FUN = devyr))%>% 
  mutate(predyr24 = sapply(predicates24,FUN = devyr))%>% 
  mutate(predyr25 = sapply(predicates25,FUN = devyr))%>% 
  mutate(predyr26 = sapply(predicates26,FUN = devyr))%>% 
  mutate(predyr27 = sapply(predicates27,FUN = devyr)) %>% 
  mutate(predyr28 = sapply(predicates28,FUN = devyr)) %>% 
  mutate(predyr29 = sapply(predicates29,FUN = devyr)) %>% 
  mutate(predyr30 = sapply(predicates30,FUN = devyr)) %>% 
  mutate(predyr31 = sapply(predicates31,FUN = devyr)) %>% 
  mutate(predyr32 = sapply(predicates32,FUN = devyr)) 

#Updating all predicate recall dates to properly show as dates
registrationjoin[124:155] <- registrationjoin[124:155] %>% 
  mutate_all(str_replace_all, "character\\(0\\)", "") %>% 
  sapply(.,FUN = asdate1)

#Adding a column to show the average date of recall by predicate, then simplifying table
registrationjoin <- registrationjoin %>% 
  mutate(avgdate = registrationjoin[124:155] %>% 
           rowMeans(na.rm = T) %>% 
           as.Date(origin = origin) %>% 
           year()) %>% 
  select(-c(2:93,95:102,105:107,114:116,119,120,121,124:155)) %>% 
  filter(numpred >0)

#Joining the recalled predicates to the recalls dataset to present more information about each recall
leftrecalljoin <- left_join(registrationjoin1,recalls,
                            by = c("device" = "results.k_numbers"))%>% 
  mutate(recalled = as.numeric(ifelse(is.na(meta.disclaimer),0,1))) %>% 
  select(-c(16:56))

##Conduct Analyses

#Setting basic values
x <- leftrecalljoin$numpred
sqrtx <- sqrt(leftrecalljoin$numpred)
y <- leftrecalljoin$recalled
date<-leftrecalljoin$avgdate

#Logistic regressions of number of recall status and predicate number
summary(glm(y~x,family = binomial()))
summary(glm(y~sqrtx,family = binomial()))
summary(glm(y~sqrtx+date,family = binomial()))

#Basic calculations for validation of sample means
recalled <- leftrecalljoin %>% 
  filter(.$recalled == 0)
notrecalled <- leftrecalljoin %>% 
  filter(.$recalled == 1)
mean(recalled$numpred)
mean(notrecalled$numpred)


## Make Visualizations

#Setting color standards
linecolors <- c("#714C02")
fillcolors <- c("#9D6C06")

#Plotting logistic regression
ggplot(as.data.frame(x,y),
       aes(x,y))+
  geom_point(position=position_jitter(h=0.1, w=0.1),
             shape = 21, alpha = 0.5, size = 2, color = 'darkblue')+
  xlab('Number of Predicates')+
  ylab('Binary Recall Variable')+
  labs(title = 'Distribution of Predicates and Recall Frequency',
       subtitle = 'Produced for API-222') +
  theme_bw()

## Write dataset to csv
write.csv(leftrecalljoin,"orthoproject.csv")

##Note: if you want to look similarly at PMAs you can use this sort of code instead of lines 59-69
pmarecalls <- recall %>% 
  filter(.$results.pma_numbers != "NULL" & .$results.pma_numbers != "N/A") %>% 
  filter(year(.$results.event_date_initiated) > 2017) %>% 
  mutate(results.pma_numbers = as.character(results.pma_numbers)) %>% 
  cSplit(., "results.k_numbers", sep = ",", direction = "long") %>% 
  mutate(results.pma_numbers = str_remove_all(results.pma_numbers,"[c\\(\\)]")) %>% 
  mutate(results.pma_numbers = unquote(results.pma_numbers)) %>% 
  #apply(.,2,as.character) %>% 
  as.data.frame()