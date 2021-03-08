rm(list = ls())


library(dplyr)
library(tibble)
library(readxl)
library(ggplot2)

# Setting the directory
setwd('/Users/nicolasraab/Dropbox/Davis/PosDoc/BordenHill/Met_Stations/') 


MetStations_All <-read.csv("Met_All_stations_out.csv")

unique(MetStations_All$Station)

MetStations_All <- MetStations_All %>% 
  mutate(Station = case_when( 
    Station == "19170 (B1R1)" ~ "B1R1",
    Station == "18331 (B1R4)" ~ "B1R4",
    Station == "17702 (B1R3)" ~ "B1R3",
    
    Station == "19120 (B2R1)" ~ "B2R1",
    Station == "18333 (B2R2)" ~ "B2R2",
    Station == "3859 (B2R3)" ~ "B2R3",
    
    Station == "18330 (B3R1)" ~ "B3R1",
    Station == "18335 (B3R2)" ~ "B3R2",
    Station == "19171 (B3R3)" ~ "B3R3",
    
    TRUE ~ as.character(Station)
  ))

unique(MetStations_All$Station)

#Changing the ColName "Station" for "Tag"; and "TIMESTAMP" for "TimeStamp"
MetStations_All <- rename(MetStations_All, Tag = Station)
MetStations_All <- rename(MetStations_All, TimeStamp = TIMESTAMP)

#Move Tag column to get it after the TimeStamp
Tag <- MetStations_All$Tag

MetStations_All <- MetStations_All %>% select(-c(Tag))
MetStations_All <- add_column(MetStations_All, Tag, .after = "TimeStamp")

##Separating by Tag
MetStation_B1R1 <- MetStations_All %>% filter(Tag == "B1R1")
MetStation_B1R4 <- MetStations_All %>% filter(Tag == "B1R4")
MetStation_B1R3 <- MetStations_All %>% filter(Tag == "B1R3")

MetStation_B2R1 <- MetStations_All %>% filter(Tag == "B2R1")
MetStation_B2R2 <- MetStations_All %>% filter(Tag == "B2R2")
MetStation_B2R3 <- MetStations_All %>% filter(Tag == "B2R3")

MetStation_B3R1 <- MetStations_All %>% filter(Tag == "B3R1")
MetStation_B3R2 <- MetStations_All %>% filter(Tag == "B3R2")
MetStation_B3R3 <- MetStations_All %>% filter(Tag == "B3R3")


##################################################################
##Merging MetData with Thermocouples

B1R1_TC <- read.csv('/Users/nicolasraab/Dropbox/Davis/PosDoc/BordenHill/Met_Stations/Thermocouples/Outputs/B1R1.csv')
B1R1_All <- merge(MetStation_B1R1,B1R1_TC,by='TimeStamp',all = TRUE)

write.csv(B1R1_All,'./Outputs_Beth/B1R1_all.csv',row.names = FALSE)

####
B1R4_TC <- read.csv('/Users/nicolasraab/Dropbox/Davis/PosDoc/BordenHill/Met_Stations/Thermocouples/Outputs/B1R4.csv')
B1R4_All <- merge(MetStation_B1R4,B1R4_TC,by='TimeStamp',all = TRUE)

write.csv(B1R4_All,'./Outputs_Beth/B1R4_all.csv',row.names = FALSE)

####
B1R3_TC <- read.csv('/Users/nicolasraab/Dropbox/Davis/PosDoc/BordenHill/Met_Stations/Thermocouples/Outputs/B1R3.csv')
B1R3_All <- merge(MetStation_B1R3,B1R3_TC,by='TimeStamp',all = TRUE)

write.csv(B1R3_All,'./Outputs_Beth/B1R3_all.csv',row.names = FALSE)


####
B2R1_TC <- read.csv('/Users/nicolasraab/Dropbox/Davis/PosDoc/BordenHill/Met_Stations/Thermocouples/Outputs/B2R1.csv')
B2R1_All <- merge(MetStation_B2R1,B2R1_TC,by='TimeStamp',all = TRUE)

write.csv(B2R1_All,'./Outputs_Beth/B2R1_all.csv',row.names = FALSE)

####
B2R2_TC <- read.csv('/Users/nicolasraab/Dropbox/Davis/PosDoc/BordenHill/Met_Stations/Thermocouples/Outputs/B2R2.csv')
B2R2_All <- merge(MetStation_B2R2,B2R2_TC,by='TimeStamp',all = TRUE)

write.csv(B2R2_All,'./Outputs_Beth/B2R2_all.csv',row.names = FALSE)

####
B2R3_TC <- read.csv('/Users/nicolasraab/Dropbox/Davis/PosDoc/BordenHill/Met_Stations/Thermocouples/Outputs/B2R3.csv')
B2R3_All <- merge(MetStation_B2R3,B2R3_TC,by='TimeStamp',all = TRUE)

write.csv(B2R3_All,'./Outputs_Beth/B2R3_all.csv',row.names = FALSE)

####
B3R1_TC <- read.csv('/Users/nicolasraab/Dropbox/Davis/PosDoc/BordenHill/Met_Stations/Thermocouples/Outputs/B3R1.csv')
B3R1_All <- merge(MetStation_B3R1,B3R1_TC,by='TimeStamp',all = TRUE)

write.csv(B3R1_All,'./Outputs_Beth/B3R1_all.csv',row.names = FALSE)

####
B3R2_TC <- read.csv('/Users/nicolasraab/Dropbox/Davis/PosDoc/BordenHill/Met_Stations/Thermocouples/Outputs/B3R2.csv')
B3R2_All <- merge(MetStation_B3R2,B3R2_TC,by='TimeStamp',all = TRUE)

write.csv(B3R2_All,'./Outputs_Beth/B3R2_all.csv',row.names = FALSE)

####
B3R3_TC <- read.csv('/Users/nicolasraab/Dropbox/Davis/PosDoc/BordenHill/Met_Stations/Thermocouples/Outputs/B3R3.csv')
B3R3_All <- merge(MetStation_B3R3,B3R3_TC,by='TimeStamp',all = TRUE)

write.csv(B3R3_All,'./Outputs_Beth/B3R3_all.csv',row.names = FALSE)



