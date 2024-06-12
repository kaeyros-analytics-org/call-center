library(stringr)
library(writexl)
library(readxl)
library(dplyr)
library(purrr)
library(cld3)
library(textcat)
library(fastText)
library(jsonlite)
library(dplyr)
library(officer)
library(flextable)
library(taskscheduleR)

root <- getwd()
path_to_transform_txt_to_xlsx <- file.path(root, "to_transform_txt_to_xlsx.R")
path_to_to_extra_info <- file.path(root, "to_extra_info.R")

source(path_to_transform_txt_to_xlsx)
source(path_to_to_extra_info)

path_data <- file.path(root, "data")
folder_path <- file.path(path_data, "conversationsAFB2023")

# Créer le répertoire de sortie des fichier xlsx s'il n'existe pas
output_folder <- file.path(path_data, "XLSX_conversationsAFB2023")
if (file.exists(output_folder)) {
  # Si le répertoire existe, le supprimer
  unlink(output_folder, recursive = TRUE)
}
dir.create(output_folder, recursive = TRUE)


# Transformation des documents txt
apply_extract_message_info(folder_path)

# # Fusionner tous les fichiers Excel en un seul
# merge_excel_files(output_folder, file.path(path_data, "merged_data_2023.xlsx"))
#
# # Importation de merged_data_2023.
# merged_data_2023 <- read_excel(file.path(path_data, "merged_data_2023.xlsx"))




############# Afin d'enregistrer les fichier xlsx traité dans la repertoire ######################
# Créer le répertoire de sortie des fichier xlsx traité s'il n'existe pas
 output_folder <- file.path(path_data, "info_conversationsAFB2023")
if (file.exists(output_folder)) {
  # Si le répertoire existe, le supprimer
  unlink(output_folder, recursive = TRUE)
}
dir.create(output_folder, recursive = TRUE)


folder_path <- file.path(path_data, "XLSX_conversationsAFB2023")

# extraction effective des informations
extract_info(folder_path)


# Fusionner tous les fichiers Json en un seul
merge_non_empty_json_files(output_folder, file.path(path_data, "info_data_AFB2023.json"))




#####################   Base de données de connaissance pour intercom  ########################
info_data_AFB2023 <- fromJSON(file.path(path_data, "info_data_AFB2023.json"))

# Créer un nouveau dataframe pour stocker les résultats
new_df <- data.frame(
  Questions = character(),
  Reponses = character(),
  stringsAsFactors = FALSE
)

# Boucler à travers chaque ligne du dataframe original
for (i in 1:nrow(info_data_AFB2023)) {
  # Extraire les données pertinentes
  row_data <- info_data_AFB2023[i, ]

  # Vérifier s'il y a une discussion client-agent pour cette ligne
  if (length(row_data$customer_agent_discussion[[1]]) > 0) {
    discussion <- row_data$customer_agent_discussion[[1]]

    # Initialiser les variables pour accumuler les questions
    current_question <- ""
    current_response <- ""

    for (j in 1:nrow(discussion)) {
      message_type <- discussion$key[j]
      message_value <- discussion$value[j]

      # Ignorer les valeurs manquantes
      if (!is.na(message_type)) {
        if (message_type == "client") {
          # Ajouter les messages des clients à la question actuelle
          if (current_question != "") {
            current_question <- paste(current_question, message_value, sep = " ")
          } else {
            current_question <- message_value
          }
        } else if (message_type == "agent") {
          # Ajouter la réponse de l'agent et créer une nouvelle ligne
          current_response <- message_value

          # Créer une nouvelle ligne dans le nouveau dataframe
          new_row <- data.frame(
            Questions = current_question,
            Reponses = current_response,
            stringsAsFactors = FALSE
          )

          # Ajouter la nouvelle ligne au nouveau dataframe
          new_df <- rbind(new_df, new_row)

          # Réinitialiser les variables pour la prochaine paire question-réponse
          current_question <- ""
          current_response <- ""
        }
      }
    }
  } else {
    # S'il n'y a pas de discussion, ajouter simplement la ligne avec des Questions et Réponses vides
    new_row <- data.frame(
      Questions = "",
      Reponses = "",
      stringsAsFactors = FALSE
    )
    new_df <- rbind(new_df, new_row)
  }
}

# Filtrer les lignes avec des Questions et Réponses non vides
new_df <- new_df[new_df$Questions != "" & new_df$Reponses != "", ]

# Créer un document Word et ajouter le dataframe
set_flextable_defaults(
  font.size = 11, font.family = 'Maiandra GD',
  font.color = "#000000",
  table.layout = "fixed",
  border.color = "#000000",
  theme_fun = "theme_box",
  padding.top = 3, padding.bottom = 3,
  padding.left = 4, padding.right = 4,
  text.align = "justify")

tab <- flextable(new_df[1:100,])
tab <- bg(tab, bg = "#0094ff", part = "header")
#tab <- autofit(tab)

# Créer un document Word et y ajouter le tableau
doc <- read_docx()
doc <- body_add_flextable(doc, set_table_properties(tab,width=1, layout = "autofit"))

# Enregistrer le document Word dans le répertoire spécifié
print(doc, target = "question_reponses.docx")


# pour automatiser les tâche de prétraitement des données
# taskscheduler_create(taskname = "knowledge_task", rscript = file.path(root, "preprossing_2023.R"),
#                      schedule = "MINUTE", starttime = format(Sys.time() + 50, "%H:%M"))
# # View(taskscheduler_ls())
#
# # pour supprimer la tache
# taskscheduler_delete(taskname = "knowledge_task")
