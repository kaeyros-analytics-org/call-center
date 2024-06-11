library(jsonlite)
library(lubridate)



#import data
root <- getwd()
path_data <- file.path(root,"data")
path_data_AFB23 <- file.path(path_data,"info_data_AFB2023.json")
# read data2
info_data_AFB2023 <- fromJSON(path_data_AFB23)
info_data_AFB2023$Start_time_discusion <- ymd_hms(info_data_AFB2023$Start_time_discusion)
info_data_AFB2023$End_time_discusion <- ymd_hms(info_data_AFB2023$End_time_discusion)
info_data_AFB2023$Start_time_chatbot <- ymd_hms(info_data_AFB2023$Start_time_chatbot)
info_data_AFB2023$End_time_chatbot <- ymd_hms(info_data_AFB2023$End_time_chatbot)
info_data_AFB2023$Start_time_agent <- ymd_hms(info_data_AFB2023$Start_time_agent)
info_data_AFB2023$End_time_agent <- ymd_hms(info_data_AFB2023$End_time_agent)

resquest_analysis_data <- info_data_AFB2023


####### data to display Total Chat Entry ##########


###### data to display total chat duration #########
# Calculer la différence entre les temps de fin et de début de discussion en secondes
diff_seconds <- as.numeric(difftime(resquest_analysis_data$End_time_discusion, resquest_analysis_data$Start_time_discusion, units = "secs"))
resquest_analysis_data$duration_chat_s <- diff_seconds


##### Data to display request_per_day_hour ######
resquest_analysis_data$request_per_day_hour <- resquest_analysis_data[c("Start_time_discusion")]

resquest_analysis_data$time <- format(resquest_analysis_data$Start_time_discusion, format = "%H:%M:%S")

resquest_analysis_data$time <- as.POSIXct(resquest_analysis_data$time, format = "%H:%M:%S")

# Définir les bornes des intervalles
intervals <- seq.POSIXt(min(resquest_analysis_data$time), max(resquest_analysis_data$time) + 1800, by = "30 min")
labels <- format(intervals, "%H:%M")
# Créer la colonne time_period basée sur les intervalles
resquest_analysis_data$time_period <- cut(resquest_analysis_data$time, breaks = intervals, labels = head(labels, -1))

# Créer la colonne date contenant le jour de la semaine correspondant
Sys.setlocale("LC_TIME", "C")
resquest_analysis_data$date <- weekdays(resquest_analysis_data$Start_time_discusion)


# Selection des variables pertinante pour le display
resquest_analysis_data <- resquest_analysis_data[c("Start_time_discusion","duration_chat_s","time_period","date")]

# Convertir le dataframe en JSON
resquest_analysis_data <- toJSON(resquest_analysis_data)

# Spécifier le chemin et le nom de fichier pour l'exportation
output_file <- file.path(path_data,"resquest_analysis_data","resquest_analysis_data.json")

# Exporter le JSON
writeLines(resquest_analysis_data, output_file)

