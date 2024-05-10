################ clean workspace
rm(list = ls())
cat("\f")

#Loading required libraries
# library(rJava)
# library(shiny)
# library(shiny.fluent)
# library(shinymanager)
# library(reactable)
# library(sf)
# library(shinyWidgets)
# library(markdown)
# library(stringr)
# library(leaflet)
# library(plotly)
# library(DT)
# library(shinycssloaders)
# library(pool)
# library(readxl)
# library(shinyjs)
# library(openxlsx)
# library(glue)
# library(rintrojs)
# library(shinyjs)
# library(dplyr)
# library(officer)
# library(lubridate)
# library(flextable)
# library(keyring)
# library(shiny)
# library(shinydashboard)
# library(daterangepicker)
# library(jsonlite)
# library(lubridate)



filterStates <- reactiveValues(
  # dataset
  dataNavi = list(dataset = "Home"),
  allDataset = NULL,
  allSubItem = NULL,
  countrySelected = "Cameroun",
  citySelected = "Yaoundé",
  date_start = "2023-1-1",
  date_end = "2023-10-30",
  filterButton = FALSE
)

# load modules and function ####
eval(parse('./modules/filter_section.R', encoding="UTF-8"))
eval(parse('./modules/bs4_tooltip.R', encoding="UTF-8"))

################ Load loginc modules
eval(parse('./modules/resquest_analysis.R', encoding="UTF-8"))
eval(parse('./modules/call_sentiments.R', encoding="UTF-8"))
eval(parse('./modules/resolution_metrics.R', encoding="UTF-8"))
eval(parse('./modules/customer_interaction.R', encoding="UTF-8"))

# visualization modules ###
eval(parse('./modules/mainContent.R', encoding="UTF-8"))
eval(parse('./modules/mainContent_landingPage.R', encoding="UTF-8"))

# header modules ###
eval(parse('./modules/headerFeedbackModal.R', encoding='UTF-8'))
eval(parse('./modules/headerMethodsModal.R', encoding='UTF-8'))
eval(parse('./modules/headerDataModal.R', encoding='UTF-8'))
eval(parse('./modules/headerWalkthrough.R', encoding='UTF-8'))
eval(parse('./modules/headerFormModal.R', encoding='UTF-8'))
eval(parse('./modules/headerFormModal_form.R', encoding='UTF-8'))
eval(parse('./modules/headerFormRatingControl.R', encoding='UTF-8'))
eval(parse('./modules/headerFormCheckboxCols.R', encoding='UTF-8'))
eval(parse('./modules/headerFormTextControl.R', encoding='UTF-8'))
eval(parse('./modules/headerFormBoxControl.R', encoding='UTF-8'))
eval(parse('./modules/headerFormRadioControl.R', encoding='UTF-8'))


# Text template loading ####
helpText <- readxl::read_xlsx("./www/templates/help_template.xlsx")

# modalXLargeDialog ####
modalXLargeDialog <- function(..., title = NULL, footer = modalButton("Dismiss"),
                              easyClose = FALSE, fade = TRUE) {
  
  cls <- if (fade) "modal fade" else "modal"
  div(id = "shiny-modal", class = cls, tabindex = "-1",
      `data-backdrop` = if (!easyClose) "static",
      `data-keyboard` = if (!easyClose) "false",
      
      div(
        class = "modal-dialog modal-xl",
        div(class = "modal-content",
            if (!is.null(title)) div(class = "modal-header",
                                     tags$h4(class = "modal-title", title)
            ),
            div(class = "modal-body", ...),
            if (!is.null(footer)) div(class = "modal-footer", footer)
        )
      ),
      tags$script("$('#shiny-modal').modal().focus();")
  )
}

# importation fichier r
source("customer_agent_en.r")

#import data
# root <- getwd()
# path_data <- file.path(root,"data")
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


# Exporter les données vers un fichier CSV
#write.csv(data, file = path_data, row.names = FALSE)

####### data to display Total Chat Entry ##########


###### data to display total chat duration #########
# Calculer la différence entre les temps de fin et de début de discussion en secondes
diff_seconds <- as.numeric(difftime(data$End_time_discusion, data$Start_time_discusion, units = "secs"))
data$duration_chat_s <- diff_seconds


##### Data to display request_per_day_hour ######
data_per_day_hour <- data[c("Start_time_discusion")]

data_per_day_hour$time <- format(data_per_day_hour$Start_time_discusion, format = "%H:%M:%S")

data_per_day_hour$time <- as.POSIXct(data_per_day_hour$time, format = "%H:%M:%S")

# Définir les bornes des intervalles
intervals <- seq.POSIXt(min(data_per_day_hour$time), max(data_per_day_hour$time) + 1800, by = "30 min")
labels <- format(intervals, "%H:%M")
# Créer la colonne time_period basée sur les intervalles
data_per_day_hour$time_period <- cut(data_per_day_hour$time, breaks = intervals, labels = head(labels, -1))

# Créer la colonne date contenant le jour de la semaine correspondant
Sys.setlocale("LC_TIME", "C")
data_per_day_hour$date <- weekdays(data_per_day_hour$Start_time_discusion)



##### Data to display request_per_month ######
data_request_per_month <- data[c("Start_time_discusion")]



######## Data to display call_answer_rate #############
# data_call_answer_rate <- data[c("customer_agent_discussion")]


####### Data to display first response time ###############
data_first_response <- data[c("id","Start_time_discusion","End_time_chatbot","Start_time_agent","End_time_agent")]
# Pour ce cas je ne doit pas considerer les chats du weekend, aussi je dois considere les echange d'une même journée entre client et agent
#comment donc tenir compte des reglamation des client faite la nuit (je suggere de supposer que le chat à eté initier le lendemain matin)
# - Je vais suppose que les End_time_chatbot de 00h à 07h30  ont débuté à 7h30


data_first_response_same_day <- data_first_response %>%
  filter(weekdays(End_time_chatbot) != "Saturday" & weekdays(End_time_chatbot) != "Sunday") %>% # exclusion des chat effectuer le weekend
  filter(date(End_time_chatbot) == date(Start_time_agent)) %>% # consideration des echange du meme jour
  mutate(End_time_chatbot = if_else(hour(End_time_chatbot) < 7 | (hour(End_time_chatbot) == 7 & minute(End_time_chatbot) < 30),
                                    ymd_hms(paste0(format(End_time_chatbot, "%Y-%m-%d"), " 07:30:00")),
                                    End_time_chatbot)) # Ramener les End_time_chatbot compris entre 00h00 et 7h30 à 7h30


data_first_response_diff_day <- data_first_response %>%
  filter(weekdays(End_time_chatbot) != "Saturday" & weekdays(End_time_chatbot) != "Sunday") %>% # exclusion des chat effectuer le weekend
  filter(date(End_time_chatbot) != date(Start_time_agent)) %>%   # consideration des echanges efffectuer au jour different
  mutate(End_time_chatbot = ymd_hms(paste0(format(Start_time_agent, "%Y-%m-%d"), " 07:30:00"))) # Ramener les End_time_chatbot compris entre 00h00 et 7h30 à 7h30



data_first_response_same_sat <- data_first_response %>%
  filter(weekdays(End_time_chatbot) == "Saturday") %>% # les chats effectuer uniquement le samedi
  filter(as.Date(Start_time_agent) == as.Date(End_time_chatbot)) %>% # consideration des echange du meme jour
  mutate(End_time_chatbot = if_else(hour(End_time_chatbot) < 7 | (hour(End_time_chatbot) == 7 & minute(End_time_chatbot) < 30),
                                    ymd_hms(paste0(format(End_time_chatbot, "%Y-%m-%d"), " 07:30:00")),
                                    End_time_chatbot)) # Ramener les End_time_chatbot compris entre 00h00 et 7h30 

 
data_first_response_diff_sat <- data_first_response %>%
  filter(weekdays(End_time_chatbot) == "Sunday") %>%    # consideration des echanges efffectuer au jour different
  filter(as.Date(Start_time_agent) != as.Date(End_time_chatbot)) %>%
  mutate(End_time_chatbot = ymd_hms(paste0(format(Start_time_agent, "%Y-%m-%d"), " 07:30:00"))) # Ramener les End_time_chatbot compris entre 00h00 et 7h30 


data_first_response_bon = rbind.data.frame(data_first_response_same_day, data_first_response_diff_day, data_first_response_same_sat, data_first_response_diff_sat)

data_first_response_bon$duration_chat_s  <- as.numeric(difftime(data_first_response_bon$Start_time_agent, data_first_response_bon$End_time_chatbot, units = "secs"))

data_first_response_bon$hours <- round(data_first_response_bon$duration_chat_s / 3600, 2)

data_first_response_bon$Start_time_discusion <- as.Date(data_first_response_bon$Start_time_discusion)



####### Data to display full response time ###############
data_full_response <- data_first_response_bon
data_full_response$duration_chat_s  <- as.numeric(difftime(data_full_response$End_time_agent, data_full_response$End_time_chatbot, units = "secs"))
data_full_response$hours <- round(data_full_response$duration_chat_s / 3600, 2)
data_full_response$Start_time_discusion <- as.Date(data_full_response$Start_time_discusion)


################# Data to display average time call by agent ###############
# Filter out rows with "auto" or empty Agent values and extract the last word of Agent
data_average_call_agent <- data %>%
  filter(Agent != "auto" & Agent != "") %>%
  mutate(Agent = tolower(word(Agent, -1)))

# Select relevant columns
data_average_call_agent <- data_average_call_agent[c("Agent", "Start_time_discusion", "Start_time_agent", "End_time_agent")]

# Calculate the call duration in minutes
data_average_call_agent$call_duration <- as.numeric(difftime(data_average_call_agent$End_time_agent, 
                                                             data_average_call_agent$Start_time_agent, 
                                                             units = "mins"))


# # Calculate the average call duration and number of calls per agent
# average_call_duration_with_count <- data_average_call_agent %>%
#   group_by(Agent) %>%
#   summarise(avg_call_duration = mean(call_duration),
#             num_calls = n()) %>%
#   arrange(desc(avg_call_duration))

# Create a bar chart using Plotly
# plot_ly(average_call_duration_with_count, x = ~avg_call_duration , y = ~Agent, type = "bar", 
#         marker = list(color = colorRampPalette(c("gray80", "gray20", "red"))(n = length(unique(average_call_duration_with_count$Agent)))),
#         text = ~paste("Agent: ", Agent, "<br>Call Duration: ", round(avg_call_duration , 2), " mins",
#                       "<br>Number of Calls: ", num_calls),
#         hoverinfo = "text") %>%
#   layout(title = "",
#          xaxis = list(title = "Agent"),
#          yaxis = list(title = "Average Call Duration (minutes)"))



####################### Analyse des scenarios ############################

############## Scenario en anglais
path_data_en_AFB23 <- file.path(path_data,"en_info_data_AFB2023.json")
# read data2
info_data_en_AFB2023 <- fromJSON(path_data_en_AFB23)
# info_data_en_AFB2023$Start_time_discusion <- ymd_hms(info_data_en_AFB2023$Start_time_discusion)
# info_data_en_AFB2023$End_time_discusion <- ymd_hms(info_data_en_AFB2023$End_time_discusion)
# info_data_en_AFB2023$Start_time_chatbot <- ymd_hms(info_data_en_AFB2023$Start_time_chatbot)
# info_data_en_AFB2023$End_time_chatbot <- ymd_hms(info_data_en_AFB2023$End_time_chatbot)
# info_data_en_AFB2023$Start_time_agent <- ymd_hms(info_data_en_AFB2023$Start_time_agent)
# info_data_en_AFB2023$End_time_agent <- ymd_hms(info_data_en_AFB2023$End_time_agent)

data_en <- info_data_en_AFB2023


# Nettoyage des données en supprimant les valeurs manquantes
data_clean <- data_en$Scenario_Chatbot[!is.na(data_en$Scenario_Chatbot)]

# Compter les occurrences de chaque scénario
data_scenario_counts <- as.data.frame(table(unlist(data_clean)))
scenario_counts <- table(unlist(data_clean))
# Créer un dataframe à partir des résultats
scenario_df <- data.frame(Scenario = names(scenario_counts), Count = as.numeric(scenario_counts))

# Filtrer les scénarios qui commencent par un chiffre suivi de deux points
scenario_df <- scenario_df[grep("^\\d{1,}: ", scenario_df$Scenario), ]

# Réinitialiser les index du dataframe
rownames(scenario_df) <- NULL


# Retirer les chiffres, les deux points et le mot "pour" au début de chaque scénario
scenario_df$Scenario <- gsub("^(\\d{1,}: |for )", "", scenario_df$Scenario)



# Trouver les indices des scénarios contenant le terme "Tapez"
indices <- grep("Press", scenario_df$Scenario)

# Supprimer les scénarios correspondants du dataframe
scenario_df <- scenario_df[-indices, ]

# Réinitialiser les index du dataframe
rownames(scenario_df) <- NULL




# Trier les données par nombre d'occurrences
scenario_df <- scenario_df[order(scenario_df$Count, decreasing = TRUE), ]

# Sélectionner les 30 premières lignes du dataframe trié
top_30 <- head(scenario_df, 30)

# # Créer un graphique interactif avec Plotly
# plot_ly(scenario_df, x = ~Count, y = ~Scenario, type = 'bar', orientation = 'h') %>%
#   layout(title = "Fréquence des scénarios de chatbot",
#          xaxis = list(title = "Nombre d'occurrences"),
#          yaxis = list(title = "Scénario"))


# 
# 
# ############## Scenario en francais 
# path_data_fr_AFB23 <- file.path(path_data,"fr_info_data_AFB2023.json")
# # read data2
# info_data_fr_AFB2023 <- fromJSON(path_data_fr_AFB23)
# # info_data_fr_AFB2023$Start_time_discusion <- ymd_hms(info_data_en_AFB2023$Start_time_discusion)
# # info_data_fr_AFB2023$End_time_discusion <- ymd_hms(info_data_en_AFB2023$End_time_discusion)
# # info_data_fr_AFB2023$Start_time_chatbot <- ymd_hms(info_data_en_AFB2023$Start_time_chatbot)
# # info_data_en_AFB2023$End_time_chatbot <- ymd_hms(info_data_en_AFB2023$End_time_chatbot)
# # info_data_en_AFB2023$Start_time_agent <- ymd_hms(info_data_en_AFB2023$Start_time_agent)
# # info_data_en_AFB2023$End_time_agent <- ymd_hms(info_data_en_AFB2023$End_time_agent)
# 
# data_fr <- info_data_fr_AFB2023
# 
# 
# # Nettoyage des données en supprimant les valeurs manquantes
# data_clean_fr <- data_fr$Scenario_Chatbot[!is.na(data_fr$Scenario_Chatbot)]
# 
# # Compter les occurrences de chaque scénario
# data_scenario_counts <- as.data.frame(table(unlist(data_clean_fr)))
# scenario_counts <- table(unlist(data_clean_fr))
# # Créer un dataframe à partir des résultats
# scenario_df <- data.frame(Scenario = names(scenario_counts), Count = as.numeric(scenario_counts))
# 
# # Filtrer les scénarios qui commencent par un chiffre suivi de deux points
# scenario_df <- scenario_df[grep("^\\d{1,}: ", scenario_df$Scenario), ]
# # Réinitialiser les index du dataframe
# rownames(scenario_df) <- NULL
# 
# # Retirer les chiffres, les deux points et le mot "pour" au début de chaque scénario
# scenario_df$Scenario <- gsub("^(\\d{1,}: |pour )", "", scenario_df$Scenario)
# 
# 
# 
# # Trouver les indices des scénarios contenant le terme "Tapez"
# indices <- grep("Tapez", scenario_df$Scenario)
# 
# # Supprimer les scénarios correspondants du dataframe
# scenario_df <- scenario_df[-indices, ]
# 
# # Réinitialiser les index du dataframe
# rownames(scenario_df) <- NULL
# 
# # Trier les données par nombre d'occurrences
# scenario_df <- scenario_df[order(scenario_df$Count, decreasing = TRUE), ]
# 
# # Sélectionner les 30 premières lignes du dataframe trié
# top_30 <- head(scenario_df, 30)
# 
# # Créer un graphique interactif avec Plotly
# 
# 
# 
# ############# Exploitation des échanges agents et clients ##########################
# # 
# # data_en$customer_agent_discussion[2]
# # 
# # data_en$customer_agent_discussion_clean <- Filter(function(x) length(x) > 0, data_en$customer_agent_discussion)
# 
