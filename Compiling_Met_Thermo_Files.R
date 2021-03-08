rm(list = ls())

library(dplyr)
library(tibble)
library(readxl)
library(lubridate)
library(stringr)
library(ggplot2)
library(reshape2)


# Setting the directory
setwd('/Volumes/GoogleDrive/.shortcut-targets-by-id/1nZJ5mio6y7uJNeOUW67s5cACfRUox3c-/Borden_stations_2020') #Directory where the raw data is in Google Drive
Dir <- getwd()

# Setting a Directory for the outcome tables
Dir_Outcome <- c("/Volumes/GoogleDrive/My Drive/BordenHill_Outputs/")



#Establish the list of stations there are supposed to be in the folder and in the field
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!! MAKE SURE THIS IS CORRECT  !!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
Stations_List <- c("B1R1","B1R4","B1R3",
                   "B2R1","B2R2","B2R3",
                   "B3R1","B3R2","B3R3")

#looking for the right directory for each stations listed above
Dir_List <- list.dirs(path = Dir, full.names = TRUE, recursive = TRUE)


for (i in 1:length(Stations_List)){
  
  #all the folders containing data on station[i]
  Folder_Station <- Dir_List[grep(Stations_List[i],Dir_List)]
  if (length(grep("lab",Folder_Station)) > 0){
  Folder_Station <- Folder_Station[-grep("lab",Folder_Station)]} #There are some folders that were lab experiments that we are not interested on
  
  
  #all the folders containing only converted data on station[i]
  Folder_Station_Converted <- Folder_Station[grep("converted",Folder_Station)]
  
  
  Table_IRT_HF <- NULL
  Table_Met <- NULL
  Table_Thermocouples <- NULL
  Table_BB <- NULL
  
  for (j in 1:length(Folder_Station_Converted)){
    
    Working_Folder <- Folder_Station_Converted[j]
    Dat_Files <- list.files(Working_Folder,pattern='[.]dat')
    setwd(Working_Folder)
    
    #Table IRT
    if(length(grep("Table_IRT_HF",Dat_Files)) != 0){  #Checking if there's table in the folder
    Table_IRT_HF[[j]] <- read.table(Dat_Files[grep("Table_IRT_HF",Dat_Files)],sep =",", skip = 1, header = TRUE)
    Table_IRT_HF[[j]] <- (Table_IRT_HF[[j]][-c(1,2),])  #deleting sub headers
    
    }else{print(paste("No Table IRT in ",Working_Folder))}
    
    #Table Meteorology
    if(length(grep("Table_Met",Dat_Files)) != 0){ #Checking if there's table in the folder
    Table_Met[[j]] <- read.table(Dat_Files[grep("Table_Met",Dat_Files)],sep =",", skip = 1, header = TRUE)
    Table_Met[[j]] <- (Table_Met[[j]][-c(1,2),])  #deleting sub headers
    
    }else{print(paste("No Table Met in ",Working_Folder))}
    
    #Table Thermocouples
    if(length(grep("Table_Thermocouples",Dat_Files)) != 0){ #Checking if there's table in the folder
    Table_Thermocouples[[j]] <- read.table(Dat_Files[grep("Table_Thermocouples",Dat_Files)],sep =",", skip = 1, header = TRUE)
    Table_Thermocouples[[j]] <- (Table_Thermocouples[[j]][-c(1,2),])  #deleting sub headers
    
    }else{print(paste("No Table Thermocouples in ",Working_Folder))} 
    
    #Table Anemometer
    if(length(grep("Table_BB",Dat_Files)) != 0){ #Checking if there's table in the folder
      Table_BB[[j]] <- read.table(Dat_Files[grep("Table_BB",Dat_Files)],sep =",", skip = 1, header = TRUE)
      Table_BB[[j]] <- (Table_BB[[j]][-c(1,2),])  #deleting sub headers
      
    }else{print(paste("No Table Anemometer in ",Working_Folder))} 
    
  }
  
  #Deleting NULL elements in each of the tables (for instance, there was no Thermocouple Table in D1, therefore NULL)
  Table_IRT_HF <- Table_IRT_HF[!sapply(Table_IRT_HF,is.null)]
  Table_Met <- Table_Met[!sapply(Table_Met,is.null)]
  Table_Thermocouples <- Table_Thermocouples[!sapply(Table_Thermocouples,is.null)]
  Table_BB <- Table_BB[!sapply(Table_BB,is.null)]
  
  #Creating a single dataframes for each table, for each site, from the individual extraction date tables
  df_Table_IRT <- do.call(bind_rows, Table_IRT_HF)
  df_Table_Met <- do.call(bind_rows, Table_Met)
  df_Table_Thermocouples <- do.call(bind_rows, Table_Thermocouples)
    if(is.null(Table_BB) == FALSE){ #Not all the stations have an Anemometer, only doing this for stations that have one
  df_Table_BB <- do.call(bind_rows, Table_BB)}
  
  
  #Sorting dataframes per date
  df_Table_Thermocouples <- df_Table_Thermocouples %>% arrange(TIMESTAMP)
  df_Table_Met <- df_Table_Met %>% arrange(TIMESTAMP) 
  df_Table_Thermocouples <- df_Table_Thermocouples %>% arrange(TIMESTAMP)
  if(is.null(Table_BB) == FALSE){ #Not all the stations have an Anemometer, only doing this for stations that have one
    df_Table_BB <- df_Table_BB %>% arrange(TIMESTAMP)}
  
  
  #Writing R files for the different tables (The Tables are too long to be written in csv)
  name_IRT <- paste(Stations_List[i],"IRT",sep = "_")
  assign(name_IRT,df_Table_IRT)
  save(list = name_IRT,file = paste(paste(paste0(Dir_Outcome,"Table_IRT"),Stations_List[i],sep = "_"),"Rdata",sep = "."))
  
  name_Met <- paste(Stations_List[i],"Met",sep = "_")
  assign(name_Met,df_Table_Met)
  save(list = name_Met,file = paste(paste(paste0(Dir_Outcome,"Table_Met"),Stations_List[i],sep = "_"),"Rdata",sep = "."))
  
  name_Thermocouples <- paste(Stations_List[i],"Thermocouples",sep = "_")
  assign(name_Thermocouples,df_Table_Thermocouples)
  save(list = name_Thermocouples,file = paste(paste(paste0(Dir_Outcome,"Table_Thermocouples"),Stations_List[i],sep = "_"),"Rdata",sep = "."))
  
  if(is.null(Table_BB) == FALSE){ #Not all the stations have an Anemometer, only doing this for stations that have one
    name_BB <- paste(Stations_List[i],"BB",sep = "_")
    assign(name_BB,df_Table_BB)
    save(list = name_BB,file = paste(paste(paste0(Dir_Outcome,"Table_BB"),Stations_List[i],sep = "_"),"Rdata",sep = "."))}
  
  } # end of "if" line 33


source()

