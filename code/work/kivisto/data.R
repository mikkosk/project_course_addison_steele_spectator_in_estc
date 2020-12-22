path <- "C:/Users/mikko/ESTC - HelDIg/estc_student_edition"

library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(rstudioapi)

setwd(path)

allData <- read.csv("data_output/estc_student_edition.csv",stringsAsFactors = FALSE)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

addison <- allData[which(allData$actor_id == '7413288'), ]
#	7413288 - Addison id

steele <- allData[which(allData$actor_id == '22167754'), ]
# 22167754 - Steele id

spectatorAdditional <- allData %>%
  filter(str_detect(title, '[tT]he Spectators'))

spectatorManual <- allData %>%
  filter(id %in% c("N22630", "T97981", "T97982", "T147412", "N35324", "N12282", "N471825"))

# code for manually searching
# <- allData %>%
#   filter_at(.vars = vars(title, remaining_title), .vars_predicate = any_vars(str_detect(., paste(c('[sS]pectator[^s]'), collapse="|")))) %>%
#   filter(!id %in% addison$id) %>%
#   filter(!id %in% steele$id) %>% 

spectator <- allData %>%
  filter_at(.vars = vars(title, remaining_title), .vars_predicate = any_vars(str_detect(., paste(c('[tT]he [sS]pectator[^s]', '[tT]he [sS]pectator$', '[dD]u [sS]pectateur'), collapse="|")))) %>%
  filter(is.na(finalWorkField) | finalWorkField != "383-works of benjamin franklin consisting of his life") %>%
  rbind(spectatorAdditional) %>%
  rbind(spectatorManual)

test <- allData[which(allData$finalWorkField == '6148-poetical works of joseph addison'), ]

adSt <- bind_rows(addison, steele)

palette <- c("#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD", 
             "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D", 
             "#8A7C64", "#599861", "#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7", 
             "#673770", "#D3D93E")

