################ clean workspace
# rm(list = ls())
# cat("\f")

#Loading required libraries
library(rJava)
library(shiny)
library(shiny.fluent)
library(shinymanager)
library(reactable)
library(sf)
library(shinyWidgets)
library(markdown)
library(stringr)
library(leaflet)
library(plotly)
library(DT)
library(shinycssloaders)
library(pool)
library(readxl)
library(shinyjs)
library(openxlsx)
library(glue)
library(rintrojs)
library(shinyjs)
library(dplyr)
library(officer)
library(lubridate)
library(flextable)
library(keyring)
library(shiny)
library(shinydashboard)
library(daterangepicker)
library(jsonlite)
require(quanteda)
require(quanteda.sentiment)
library(syuzhet)
library(SnowballC)
library(textstem)
library(stringr)
library(tidytext)
library(wordcloud2)
library(echarts4r)


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
#source("customer_agent_en.r")

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


data <- info_data_AFB2023[sample(nrow(info_data_AFB2023), 500), ]

# ##### Data to display resquest_analysis page ######
resquest_analysis_file <- file.path(path_data,"resquest_analysis_data","resquest_analysis_data.json")
resquest_analysis_data <- fromJSON(resquest_analysis_file)
resquest_analysis_data$Start_time_discusion <- ymd_hms(resquest_analysis_data$Start_time_discusion)
resquest_analysis_data <- resquest_analysis_data[sample(nrow(resquest_analysis_data), 500), ]


######## Data to display resolution_metrics page #############

resolution_metrics_file <- file.path(path_data,"resolution_metrics_data","resolution_metrics_data.json")
resolution_metrics_data <- fromJSON(resolution_metrics_file)
resolution_metrics_data$Start_time_discusion <- as.Date(resolution_metrics_data$Start_time_discusion)
resolution_metrics_data <- resolution_metrics_data[sample(nrow(resolution_metrics_data), 500), ]



######## Data to display customer_interaction_data page #############
data_average_call_agent_file <- file.path(path_data,"customer_interaction_data","data_average_call_agent.json")
data_average_call_agent <- fromJSON(data_average_call_agent_file)
data_average_call_agent$Start_time_discusion <- as.Date(data_average_call_agent$Start_time_discusion)
data_average_call_agent <- data_average_call_agent[sample(nrow(data_average_call_agent), 500), ]

wordcloud_data_file <- file.path(path_data,"customer_interaction_data","wordcloud_data.json")
wordcloud_data <- fromJSON(wordcloud_data_file)
wordcloud_data$Start_time_discusion <- as.Date(wordcloud_data$Start_time_discusion)
wordcloud_data <- wordcloud_data[sample(nrow(wordcloud_data), 500), ]


# ####################### Data to display scenarios ############################
# 
# ############## Scenario en anglais
# path_data_en_AFB23 <- file.path(path_data,"en_info_data_AFB2023.json")
# # read data2
# info_data_en_AFB2023 <- fromJSON(path_data_en_AFB23)
# 
# data_en <- info_data_en_AFB2023
# 
# # Nettoyage des données en supprimant les valeurs manquantes
# data_clean <- data_en$Scenario_Chatbot[!is.na(data_en$Scenario_Chatbot)]
# 
# # Compter les occurrences de chaque scénario
# data_scenario_counts <- as.data.frame(table(unlist(data_clean)))
# scenario_counts <- table(unlist(data_clean))
# # Créer un dataframe à partir des résultats
# scenario_df <- data.frame(Scenario = names(scenario_counts), Count = as.numeric(scenario_counts))
# 
# # Filtrer les scénarios qui commencent par un chiffre suivi de deux points
# scenario_df <- scenario_df[grep("^\\d{1,}: ", scenario_df$Scenario), ]
# 
# # Réinitialiser les index du dataframe
# rownames(scenario_df) <- NULL
# 
# 
# # Retirer les chiffres, les deux points et le mot "pour" au début de chaque scénario
# scenario_df$Scenario <- gsub("^(\\d{1,}: |for )", "", scenario_df$Scenario)
# 
# 
# 
# # Trouver les indices des scénarios contenant le terme "Tapez"
# indices <- grep("Press", scenario_df$Scenario)
# 
# # Supprimer les scénarios correspondants du dataframe
# scenario_df <- scenario_df[-indices, ]
# 
# # Réinitialiser les index du dataframe
# rownames(scenario_df) <- NULL







scenario_data_file <- file.path(path_data,"call_sentiments_data","scenario_data.json")
scenario_data <- fromJSON(scenario_data_file)

# Trier les données par nombre d'occurrences
scenario_data <- scenario_data[order(scenario_data$Count, decreasing = TRUE), ]
# Sélectionner les 30 premières lignes du dataframe trié
top_30 <- head(scenario_data, 30)

##################### Data to display sentiment analyst ############################
sentiment_data_file <- file.path(path_data,"call_sentiments_data","sentiment_data.json")
sentiment_data <- fromJSON(sentiment_data_file)
sentiment_data$Start_time_discusion <- as.Date(sentiment_data$Start_time_discusion)





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
