rm(list = ls())


library(dplyr)
library(tibble)
library(readxl)
library(ggplot2)
library(lubridate)
library(naniar)
library(stringr)
library(reshape2)

# Setting the directory
setwd('/Volumes/GoogleDrive/My Drive/BordenHill_Outputs/') #Folder where you find the outputs from the compiling script "Compiling_Met_Thermo_Files.R" 
Dir <- getwd()


# Setting Output directory
Dir_Output <- c("/Volumes/GoogleDrive/My Drive/BordenHill_Outputs/Met_15min/")

#Get list of files in the Outcome Directory
Dat_Files <- list.files(Dir,pattern='[.]Rdata')

#Get only Thermocouples files
Dat_Met <- Dat_Files[grep("Met",Dat_Files)]

Stations_List <- c("B1R1","B1R4","B1R3",
                   "B2R1","B2R2","B2R3",
                   "B3R1","B3R2","B3R3")

Met_All <- NULL
for(i in 1:length(Stations_List)){
  
  Met_Station <- Dat_Met[grep(Stations_List[i],Dat_Met)]
  load(Met_Station)
  
  #Changing the name of the incoming file so it can run on a loop (dropping the station prefix)
  assign("Met", eval(parse(text = ls()[grep(Stations_List[i],ls())])))
  
  #TimeStamp as date-time class
  Met$TIMESTAMP <- as.POSIXct(Met$TIMESTAMP)
  
  #Dropping IRT_Max, and IRT Time Max
  Met <- Met %>% select(-RECORD)
  
  #Adding the Site  
  Met <- add_column(Met, Station = Stations_List[i], .after = "TIMESTAMP")    
  
  Met_All[[i]] <- Met
}
  

#Deleting NUll elements from the list  
Met_All <- Met_All[!sapply(Met_All,is.null)]

#Consolidating the list
Met_All <- do.call(bind_rows,Met_All)

#Dropping columns that are trash
Met_All <- Met_All %>% select(-Table_Met,-X15.MIN,-X1004,-X599164,-X63525,-Sec100Usec,-X........,-X,-X1982858864)

#save Rdat
setwd(Dir_Output)
save(Met_All,file = "All_Met_15_min.Rdata")


Met_All <- add_column(Met_All, TimeStampSite = paste(Met_All$TIMESTAMP,Met_All$Station, sep = "-"),
                                       .before = "TIMESTAMP")


####################################################################
######################## Merging All The Tables ####################
####################################################################

#Load Thermocouples Table All
load("/Volumes/GoogleDrive/My Drive/BordenHill_Outputs/Thermocouples_15min/All_Thermocouples_15_min.Rdata")

Thermocouples_All_15_min <- add_column(Thermocouples_All_15_min, TimeStampSite = paste(Thermocouples_All_15_min$TIMESTAMP,Thermocouples_All_15_min$Station, sep = "-"),
                                       .before = "TIMESTAMP")


#Load IRT Table All
load("/Volumes/GoogleDrive/My Drive/BordenHill_Outputs/IRT_15min/All_IRT_15_min.Rdata")

IRT_All_15_min <- add_column(IRT_All_15_min, TimeStampSite = paste(IRT_All_15_min$TIMESTAMP,IRT_All_15_min$Station, sep = "-"),
                                       .before = "TIMESTAMP")


   Met_Thermo_All <- merge(Met_All, IRT_All_15_min, by.x = "TimeStampSite", by.y = "TimeStampSite", all.x = TRUE)
   
   Met_Thermo_IRT_All_plus <- merge(Met_Thermo_All, Thermocouples_All_15_min, by.x = "TimeStampSite", by.y = "TimeStampSite", all.x = TRUE)
  
   Met_Thermo_IRT_All_plus <- arrange(Met_Thermo_IRT_All_plus,Station.x)
   
   Met_Thermo_IRT_All_plus <- Met_Thermo_IRT_All_plus %>% select(-TIMESTAMP,-TIMESTAMP.y,-Station,-Station.y) 
   
   Met_Thermo_IRT_All_plus <- Met_Thermo_IRT_All_plus %>% rename(Station = Station.x, TIMESTAMP = TIMESTAMP.x)
   
   write.csv(Met_Thermo_IRT_All_plus,"/Volumes/GoogleDrive/My Drive/BordenHill_Outputs/Met_15min/BordenHill2020_All.csv", row.names = FALSE)
   
   