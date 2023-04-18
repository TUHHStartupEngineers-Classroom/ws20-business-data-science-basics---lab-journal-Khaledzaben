# Data Wrangling


# 1.0 LIBRARIES ----
library(tidyverse)
library(vroom)
library(magrittr)
library(lubridate)
library(data.table)



# 2.0 DATA IMPORT ----
patent <-  "C:/Users/Khale/Desktop/Business Module/DS_101/02_data_wrangling/patent.tsv"
patent_tbl <- fread(patent)
setnames(patent_tbl, "id", "patent_id")
patent_tbl_view <- patent_tbl[1:2]


assignee  <-   "C:/Users/Khale/Desktop/Business Module/DS_101/02_data_wrangling/assignee.tsv"
assignee_tbl <- fread(assignee)
setnames(assignee_tbl, "id", "assignee_id")


patent_assignee <- "C:/Users/Khale/Desktop/Business Module/DS_101/02_data_wrangling/patent_assignee.tsv"
patent_assignee_tbl<- fread(patent_assignee)

uspc <- "C:/Users/Khale/Desktop/Business Module/DS_101/02_data_wrangling/uspc.tsv"
uspc_tbl<- fread(uspc)
setnames(uspc_tbl, "uuid", "assignee_id")



# 1. Patent Dominance

combined_data_assignee <- merge(assignee_tbl, patent_assignee_tbl, by='assignee_id')
na.omit(combined_data_assignee, cols="organization")

#What US company has the most patents? 

combined_data_assignee [, .N, by = organization][order(-N)] %>% head(1)%>%na.omit()

#List the 10 US companies with the most assigned/granted patents.

combined_data_assignee [, .N, by = organization][order(-N)]%>%na.omit() %>% head(11)



#2. Recent patent activity

combined_data_patent <- merge(combined_data_assignee,patent_tbl , by='patent_id') 
reshaped_patent_tbl <- combined_data_patent[,.(patent_id, country, date, organization,kind)]

#What US company had the most patents granted in 2019? 

reshaped_patent_tbl [lubridate::year(date) == 2019, .N, by = organization][order(-N)]%>%na.omit() %>% head(1)

#List the top 10 companies with the most new granted patents for 2019.

reshaped_patent_tbl [lubridate::year(date) == 2019 & kind=="B1", .N, by = organization][order(-N)]%>%na.omit() %>% head(10)


# 3. Innovation in Tech

combined_data_uspc <- merge(combined_data_assignee,uspc_tbl , by='patent_id') 
reshaped_uspc_tbl <- combined_data_uspc[,.(patent_id, type, organization,mainclass_id,subclass_id)]

#What is the most innovative tech sector?

patent_tbl[, .N, by = type][order(-N)] %>% head(1)


#What are the top 5 USPTO main classes of their patents?

reshaped_uspc_tbl[organization=="International Business Machines Corporation", .N, by = mainclass_id][order(-N)]%>% head(5)


















