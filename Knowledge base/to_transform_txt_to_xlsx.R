library(stringr)
library(writexl)
library(readxl)
library(dplyr)
library(purrr)

extract_message_info <- function(path) {
  messages <- readLines(path, encoding = "latin1")  # Importer les messages depuis le fichier

  # Fonction pour extraire l'ID du nom de fichier
  extract_id <- function(path) {
    filename <- basename(path)  # Obtenir le nom de fichier à partir du chemin
    id_match <- str_match(filename, "(\\d{6})\\.")  # Extraire les 6 derniers chiffres du nom de fichier
    id <- gsub("\\.", "", id_match[1] ) # Récupérer la correspondance
    return(id)
  }

  # Initialiser les variables pour stocker les informations
  Phone <- c()
  Datetime <- c()
  Name <- c()
  Content <- c()

  # Parcourir chaque ligne du fichier
  for (line in messages) {
    # Si la ligne contient une date et une heure
    if (grepl("\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}", line)) {
      # Extraire les informations de la ligne
      phone <- str_extract(line, "\\+\\d{10,}")  # Extraction du numéro de téléphone
      datetime <- str_extract(line, "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}")  # Extraction de la date et l'heure
      name <- str_extract(line, "^[^(]+")  # Extraction du nom du participant
      content_match <- str_match(line, ": (.*)")  # Extraction du contenu de la ligne sans ":"
      content <- content_match[, 2]  # Récupération du contenu extrait

      Phone <- c(Phone, phone)
      Datetime <- c(Datetime, datetime)
      Name <- c(Name, name)
      Content <- c(Content, content)
    }
    # Si la ligne ne contient pas une date et une heure
    else {
      # Concaténer le dernier element de Content avec la ligne
      Content[length(Content)] <- paste(Content[length(Content)], line)
    }
  }

  # Supprimer les numéros de téléphone de la colonne Nom
  Name <- gsub("\\+\\d{10,}", "", Name)

  # Retourner les informations sous forme de dataframe
  df <- data.frame(
    id = extract_id(path),
    `Nom agent` = Name,
    `Numéro de téléphone` = Phone,
    `Date et heure` = Datetime,
    `Contenu du message` = Content,
    stringsAsFactors = FALSE
  )

  return(df)
}


apply_extract_message_info <- function(folder_path) {
  # Obtenir la liste des fichiers dans le dossier
  files <- list.files(folder_path, full.names = TRUE)

  # Parcourir chaque fichier
  for (file in files) {
    # Essayer d'appliquer la fonction extract_message_info à chaque fichier
    # Si une erreur se produit, afficher un message et passer au fichier suivant
    tryCatch({
      df <- extract_message_info(file)

      # Construire le chemin pour sauvegarder le fichier XLSX
      output_path <- file.path(output_folder, paste0(basename(file), ".xlsx"))

      # Sauvegarder le dataframe comme fichier XLSX
      write_xlsx(df, output_path)
    }, error = function(e) {
      message("Une erreur s'est produite lors du traitement du fichier ", basename(file), ": ", conditionMessage(e))
    })
  }
}



# Fonction pour fusionner tous les fichiers Excel en un seul
merge_excel_files <- function(output_folder, output_filename) {
  # Obtenir la liste des fichiers Excel dans le dossier
  excel_files <- list.files(output_folder, pattern = "\\.xlsx$", full.names = TRUE)

  # Lire tous les fichiers Excel en une liste de dataframes
  df_list <- map(excel_files, read_excel)

  # Fusionner tous les dataframes en un seul
  merged_df <- bind_rows(df_list)

  # Enregistrer le dataframe fusionné dans un nouveau fichier Excel
  write_xlsx(merged_df, output_filename)
}

