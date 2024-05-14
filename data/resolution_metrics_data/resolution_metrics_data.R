library(jsonlite)
library(lubridate)
library(dplyr)


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

data <- info_data_AFB2023

Sys.setlocale("LC_TIME", "C") # pour que le format des date soit de type anglais

####### Data to display first response time ###############
data_first_response <- data[c("id","Start_time_discusion","End_time_chatbot","Start_time_agent","End_time_agent","customer_agent_discussion")]
# Pour ce cas je ne doit pas considerer les chats du weekend, aussi je dois considere les echange d'une même journée entre client et agent
#comment donc tenir compte des reglamation des client faite la nuit (je suggere de supposer que le chat à eté initier le lendemain matin)
# - Je vais suppose que les End_time_chatbot de 00h à 07h30  ont débuté à 7h30


data_first_response_same_day <- data_first_response %>%
  filter(weekdays(End_time_chatbot) != "Saturday" & weekdays(End_time_chatbot) != "Sunday") %>% # exclusion des chat effectuer le weekend
  filter(date(End_time_chatbot) == date(Start_time_agent)) %>% # consideration des echange du meme jour
  mutate(End_time_chatbot = dplyr::if_else(hour(End_time_chatbot) < 7 | (hour(End_time_chatbot) == 7 & minute(End_time_chatbot) < 30),
                                    ymd_hms(paste0(format(End_time_chatbot, "%Y-%m-%d"), " 07:30:00")),
                                    End_time_chatbot)) # Ramener les End_time_chatbot compris entre 00h00 et 7h30 à 7h30


data_first_response_diff_day <- data_first_response %>%
  filter(weekdays(End_time_chatbot) != "Saturday" & weekdays(End_time_chatbot) != "Sunday") %>% # exclusion des chat effectuer le weekend
  filter(date(End_time_chatbot) != date(Start_time_agent)) %>%   # consideration des echanges efffectuer au jour different
  mutate(End_time_chatbot = ymd_hms(paste0(format(Start_time_agent, "%Y-%m-%d"), " 07:30:00"))) # Ramener les End_time_chatbot compris entre 00h00 et 7h30 à 7h30



data_first_response_same_sat <- data_first_response %>%
  filter(weekdays(End_time_chatbot) == "Saturday") %>% # les chats effectuer uniquement le samedi
  filter(as.Date(Start_time_agent) == as.Date(End_time_chatbot)) %>% # consideration des echange du meme jour
  mutate(End_time_chatbot = dplyr::if_else(hour(End_time_chatbot) < 7 | (hour(End_time_chatbot) == 7 & minute(End_time_chatbot) < 30),
                                    ymd_hms(paste0(format(End_time_chatbot, "%Y-%m-%d"), " 07:30:00")),
                                    End_time_chatbot)) # Ramener les End_time_chatbot compris entre 00h00 et 7h30 



data_first_response_diff_sat <- data_first_response %>%
  filter(weekdays(End_time_chatbot) == "Sunday") %>%    # consideration des echanges efffectuer au jour different
  filter(as.Date(Start_time_agent) != as.Date(End_time_chatbot)) %>%
  mutate(End_time_chatbot = ymd_hms(paste0(format(Start_time_agent, "%Y-%m-%d"), " 07:30:00"))) # Ramener les End_time_chatbot compris entre 00h00 et 7h30 


data_first_response_bon = rbind.data.frame(data_first_response_same_day, data_first_response_diff_day, data_first_response_same_sat, data_first_response_diff_sat)

data_first_response_bon$duration_chat_first  <- as.numeric(difftime(data_first_response_bon$Start_time_agent, data_first_response_bon$End_time_chatbot, units = "secs"))

data_first_response_bon$hours_first <- round(data_first_response_bon$duration_chat_first / 3600, 2)

#data_first_response_bon$Start_time_discusion <- as.Date(data_first_response_bon$Start_time_discusion)



####### Data to display full response time ###############
data_full_response <- data_first_response_bon
data_full_response$duration_chat_full  <- as.numeric(difftime(data_full_response$End_time_agent, data_full_response$End_time_chatbot, units = "secs"))
data_full_response$hours_full <- round(data_full_response$duration_chat_full / 3600, 2)
#data_full_response$Start_time_discusion <- as.Date(data_full_response$Start_time_discusion)


# Convertir le dataframe en JSON
resolution_metrics_data <- toJSON(data_full_response)

# Spécifier le chemin et le nom de fichier pour l'exportation
output_file <- file.path(path_data,"resolution_metrics_data","resolution_metrics_data.json")

# Exporter le JSON
writeLines(resolution_metrics_data, output_file)
