path <- "C:/Users/mikko/ESTC - HelDIg/estc_student_edition"

library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(patchwork)

setwd(path)

allData <- read.csv("data_output/estc_student_edition.csv",stringsAsFactors = FALSE)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

link <- read.csv("csv/estc_bernard_link.csv", stringsAsFactors = FALSE, sep = ";") %>% 
  tidyr::separate_rows(ï..estc) %>%
  dplyr::rename(id = ï..estc)

bernard_additional <- read.csv("csv/bernard_additional.csv", stringsAsFactors = FALSE, sep = ";") %>% dplyr::rename(id = ï..id) %>% mutate(bernard = as.integer(bernard)) %>% mutate(is_organization = as.character(is_organization))

clean <- read.csv("csv/spectator_clean.csv", stringsAsFactors = FALSE, sep = ";") %>% dplyr::rename(id = ï..id)

is_pure <- read.csv("csv/pure_spectator.csv", stringsAsFactors = FALSE, sep = ";") %>% dplyr::rename(id = ï..pure_specatator)

addison <- allData[which(allData$actor_id == '7413288'), ]
#	7413288 - Addison id

steele <- allData[which(allData$actor_id == '22167754'), ]
# 22167754 - Steele id

swift <- allData[which(allData$actor_id == "14777110"), ]

tonson <- allData[which(allData$actor_id %in% c("121291376","330654","j_r_tonson_s_draper_1","http://bbti.bodleian.ox.ac.uk/details/?traderid=116058&printer_friendly=true a_b_tonson_t_draper_1")),]

spectatorAdditional <- allData %>%
  filter(str_detect(title, '[tT]he Spectators'))

spectatorManual <- allData %>%
  filter(id %in% c("N22630", "T97981", "T97982", 
                   "T147412", "N35324", "N12282", "N471825",
                   "T89184", "N10319", "N11313", "N24149",
                   "T89169", "T89167", "T89168", "T89170",
                   "T222641", "N35672", "N26920", "N25771", 
                   "T119967", "T123215", "T131121", "T119955",
                   "N4192", "T129749", "T155092", "T118537",
                   "T117520", "T165251", "T179988", "T121516",
                   "N12117", "N40431", "T129459", "T116459", "N25210",
                   "T144935", "T167189", "N2051", "N2050",
                   "N508125", "T170346") | finalWorkField %in% c("1157-miscellaneous works in verse and prose of right honourable joseph addison in three volumes"))


# code for manually searching
# <- allData %>%
#   filter_at(.vars = vars(title, remaining_title), .vars_predicate = any_vars(str_detect(., paste(c('[sS]pectator[^s]'), collapse="|")))) %>%
#   filter(!id %in% addison$id) %>%
#   filter(!id %in% steele$id) %>% 

spectator <- allData %>%
  filter_at(.vars = vars(title, remaining_title), .vars_predicate = any_vars(str_detect(., paste(c('[tT]he [sS]pectator[^s]', '[tT]he [sS]pectator$', '[dD]u [sS]pectateur'), collapse="|")))) %>%
  filter(is.na(finalWorkField) | finalWorkField != "383-works of benjamin franklin consisting of his life") %>%
  filter(title != "The spectator of the stage") %>%
  rbind(spectatorAdditional) %>%
  rbind(spectatorManual) %>%
  filter(!id %in% clean$id) %>%
  dplyr::left_join(link, by="id") %>%
  dplyr::add_row(bernard_additional) %>%
  mutate(is_organization = as.logical(is_organization)) %>%
  mutate(pure = id %in% is_pure$id) %>%
  mutate(finalWorkField = ifelse(pure == TRUE, "spectator", finalWorkField))
  
  
#fix poor metadata
spectator$publication_year[spectator$id == "T89184"] <- 1719
spectator$publication_decade[spectator$id == "T89184"] <- 1710

distinctSpectator <- spectator %>% distinct(id)

test <- allData[which(allData$finalWorkField == '6148-poetical works of joseph addison'), ]

#remove this
find <- allData %>% filter(id == "T29780")

adSt <- bind_rows(addison, steele)

palette <- c("#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD", 
             "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D", 
             "#8A7C64", "#599861", "#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7", 
             "#673770", "#D3D93E")


