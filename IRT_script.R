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
Dir_Output <- c("/Volumes/GoogleDrive/My Drive/BordenHill_Outputs/IRT_15min/")

#Get list of files in the Outcome Directory
Dat_Files <- list.files(Dir,pattern='[.]Rdata')

#Get only Thermocouples files
Dat_IRT <- Dat_Files[grep("IRT",Dat_Files)]

Stations_List <- c("B1R1","B1R4","B1R3",
                   "B2R1","B2R2","B2R3",
                   "B3R1","B3R2","B3R3")


IRT_All_15_min <- NULL
for(i in 1:length(Stations_List)){
  
  IRT_Station <- Dat_IRT[grep(Stations_List[i],Dat_IRT)]
  load(IRT_Station)
  
  #Changing the name of the incoming file so it can run on a loop (dropping the station prefix)
  assign("IRT", eval(parse(text = ls()[grep(Stations_List[i],ls())])))
  
  
  #Checking for Temp_C the only useful variable in IRT Table, the IRT Values are already averages in Met Tables
  if(is.integer(grep("Temp_C",names(IRT))) && !length(grep("Temp_C",names(IRT))) == 0){
    
  
  #TimeStamp as date-time class
  IRT$TIMESTAMP <- as.POSIXct(IRT$TIMESTAMP)
  
  #Drop Record column
  IRT <- IRT %>% select(TIMESTAMP, Temp_C_TypeE)
  
  names_table <- names(IRT)
  
  #All columns but TimeStamp as numeric
  IRT$Temp_C_TypeE <- as.numeric(IRT$Temp_C_TypeE)
  IRT$Temp_C_TypeE[IRT$Temp_C_TypeE == "NaN"] <- NA
  IRT$Temp_C_TypeE <- as.numeric(IRT$Temp_C_TypeE)
  
  #adding a floor to have time records every 15 minutes
  IRT <- IRT %>% add_column(Floor_date = floor_date(IRT$TIMESTAMP, "15 mins"),.after = "TIMESTAMP")
  
  
  
  #Averaging at a 15 minute interval
  
  
  IRT_15min_Ave <- aggregate(IRT$Temp_C_TypeE~IRT$Floor_date,FUN=mean,na.action = na.omit)
  
  colnames(IRT_15min_Ave) <- names_table
  
  #Adding the Site  
  IRT_15min_Ave <- add_column(IRT_15min_Ave, Station = Stations_List[i], .after = "TIMESTAMP")    
  
  write.csv(IRT_15min_Ave, file = paste0(Dir_Output,paste(Stations_List[i],"IRT.csv",sep = "_")),row.names = FALSE)
  
  #Saving All Stations in one list
  IRT_All_15_min[[i]] <- IRT_15min_Ave
  
  
  
  } #end from if line 41
  
  
}
  
  #Deleting NUll elements from the list  
  IRT_All_15_min <- IRT_All_15_min[!sapply(IRT_All_15_min,is.null)]

  #Consolidating the list
  IRT_All_15_min <- do.call(bind_rows,IRT_All_15_min)

  #save Rdat
  setwd(Dir_Output)
  save(IRT_All_15_min,file = "All_IRT_15_min.Rdata")



