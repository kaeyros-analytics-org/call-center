library(stringr)
library(writexl)


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
      phone <- str_extract(line, "\\+\\d{11,}")  # Extraction du numéro de téléphone
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
  Name <- gsub("\\+\\d{11,}", "", Name)

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


# Créer le répertoire de sortie s'il n'existe pas
output_folder <- file.path(path_data, "XLSX_conversationsAFB2020_Aout")
if (!file.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

# Fonction pour parcourir les fichiers d'un dossier et appliquer la fonction extract_message_info
apply_extract_message_info <- function(folder_path) {
  # Obtenir la liste des fichiers dans le dossier
  files <- list.files(folder_path, full.names = TRUE)

  # Parcourir chaque fichier
  for (file in files) {
    # Appliquer la fonction extract_message_info à chaque fichier et stocker le résultat dans la liste
    df <- extract_message_info(file)

    # Construire le chemin pour sauvegarder le fichier RDS
    output_path <- file.path(output_folder, paste0(basename(file), ".xlsx"))

    # Save dataframe as a CSV file
    write_xlsx(df, output_path)#, row.names = FALSE, fileEncoding = "latin1")

  }
}


# utilisation
root <- getwd()
path_data <- file.path(root, "data")
folder_path <- file.path(path_data, "conversationsAFB2020_Aout")
apply_extract_message_info(folder_path)

