library(stringr)

# Fonction pour extraire les informations des messages et ajouter l'ID du fichier
extract_message_info <- function(path) {
  messages <- readLines(path, encoding = "latin1")  # Importer les messages depuis le fichier

  # Fonction pour extraire l'ID du nom de fichier
  extract_id <- function(path) {
    filename <- basename(path)  # Obtenir le nom de fichier à partir du chemin
    id_match <- str_match(filename, "\\d{6}")  # Extraire les 6 chiffres du nom de fichier
    id <- id_match[1, 1]  # Récupérer la première correspondance (la seule dans ce cas)
    return(id)
  }

  # Application de la fonction à chaque message
  message_info <- lapply(messages, function(message) {
    phone <- str_extract(message, "\\+\\d{11}")  # Extraction du numéro de téléphone
    datetime <- str_extract(message, "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}")  # Extraction de la date et l'heure
    name <- str_extract(message, "^[^(]+")  # Extraction du nom du participant
    content_match <- str_match(message, ": (.*)")  # Extraction du contenu du message sans ":"
    content <- content_match[, 2]  # Récupération du contenu extrait
    return(list(name = name, phone = phone, datetime = datetime, content = content))
  })

  # Création du dataframe
  df <- data.frame(matrix(unlist(message_info), nrow = length(message_info), byrow = TRUE))
  colnames(df) <- c("Nom", "Numéro de téléphone", "Date et heure", "Contenu du message")

  # Ajouter une colonne "id" à df
  df$id <- extract_id(path)

  # Réorganiser les colonnes pour mettre "id" en première position
  df <- df[, c("id", setdiff(colnames(df), "id"))]

  return(df)
}

# Exemple d'utilisation
root <- getwd()
path_data <- file.path(root, "data")
path_single_14182651215 <- file.path(path_data, "conversationsAFB2020_Aout", "+14182651215_2020-12-12 182825.txt")
df <- extract_message_info(path_single_14182651215)

# Save dataframe as a CSV file
write.csv(df, file.path(path_data,"CSV_conversationsAFB2020_Aout","+14182651215_2020-12-12 182825.csv"),
          row.names = FALSE, fileEncoding = "latin1")
