library(jsonlite)
library(tidyverse)
library(dotenv)

rm(list=ls())

json_path <- Sys.getenv("JSON_PATH")
cartella <- json_path

file_giri <- list.files(path = cartella, 
                             pattern = "laptimes\\.json$", 
                             full.names = TRUE, 
                             recursive = TRUE)

files_qualifiche_lap <- file_giri[grepl("Qualifying", file_giri) & !grepl("Sprint",file_giri)]
lista_dati <- list()

for(i in 1:length(files_qualifiche_lap)) {
  
  cartella_giro <- files_qualifiche_lap[i]
  
  laptimes_data <- fromJSON(cartella_giro)
    
  giro <- as.numeric(laptimes_data$lap[which.min(laptimes_data$time)])
  
  cartella_pilota <- dirname(cartella_giro)
    if(length(giro) > 0){
    path_telemetry <- paste0(cartella_pilota, "/", giro, "_tel.json")
    } else{
      giro <- 1
      path_telemetry <- paste0(cartella_pilota, "/", giro, "_tel.json") } 
    
      testo <- read_file(path_telemetry)
      testo <- str_replace_all(testo, "NaN", "null")
      json_data <- fromJSON(testo)[["tel"]]
      json_data <- as_tibble(json_data)
      
      parti_cartella <- str_split(cartella_giro, "/")[[1]]
      n <- length(parti_cartella)
      
      json_data$pilota <- parti_cartella[n-1]
      json_data$GP <- parti_cartella[n-3]
      json_data$gomma <- rep(laptimes_data$compound[giro],nrow(json_data))
      json_data$lap_time <- rep(laptimes_data$time[giro],nrow(json_data))
      json_data$strategy <- rep(laptimes_data$status[giro],nrow(json_data))
      json_data$life <- rep(laptimes_data$life[giro],nrow(json_data))
      json_data$life <- as.numeric(json_data$life)
      lista_dati[[i]] <- json_data
    }

dataset_completo <- bind_rows(lista_dati)

dataset_completo <- dataset_completo %>%
  select(-dataKey)


# setwd("C:/Users/feder/Documents/datasets/Computazionale/F1/data")
saveRDS(dataset_completo, file = Sys.getenv("DATA"))





