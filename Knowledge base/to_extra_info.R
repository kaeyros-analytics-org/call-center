library(stringr)
library(writexl)
library(readxl)
library(dplyr)
library(purrr)
library(cld3)
library(textcat)
library(fastText)
library(jsonlite)

###########################################################################################################################"
#data <- read_excel(file.path(path_data, "XLSX_conversationsAFB2023", "+12024998827_2023-03-02 152152.txt.xlsx"))
#
# # Remplacer les occurrences de "chatbot " par "chatbot" dans la colonne Nom.agent
# data$Nom.agent <- gsub("chatbot ", "chatbot", data$Nom.agent)
#
# # Identifier la langue du texte
# langue_count <- table(detect_language(data$Contenu.du.message))
# langue <- names(which.max(langue_count))
#


# Fonction pour extraire les informations personnelles
extract_personal_info <- function(content, langue) {
  if (langue == "fr") {
    Nom <- str_extract(content, "(?<=Nom\\(s\\):\\s).*?(?=</br>)")
    Prenom <- str_extract(content, "(?<=Prénom\\(s\\):\\s).*?(?=</br>)")
    Numero_de_compte <- str_extract(content, "(?<=N° de compte / carte prépayée:\\s).*?(?=</br>)")
    SARA_subscriber_number <- str_extract(content, "(?<=N° d'abonné SARA:\\s).*?(?=</br>)")
    NIC_number <- str_extract(content, "(?<=N° de CNI:\\s).*?(?=</br>)")
  } else if (langue == "en") {
    Nom <- str_extract(content, "(?<=Last name\\(s\\):\\s).*?(?=</br>)")
    Prenom <- str_extract(content, "(?<=First name\\(s\\):\\s).*?(?=</br>)")
    Numero_de_compte <- str_extract(content, "(?<=Account number/prepaid card:\\s).*?(?=</br>)")
    SARA_subscriber_number <- str_extract(content, "(?<=SARA subscriber number:\\s).*?(?=</br>)")
    NIC_number <- str_extract(content, "(?<=NIC number:\\s).*?(?=</br>)")
  }
  return(list(Nom = Nom, Prenom = Prenom, Numero_de_compte = Numero_de_compte, SARA_subscriber_number = SARA_subscriber_number, NIC_number = NIC_number))
}


#Fonction pour extraire les choix du client avec leurs modalités
extract_choix_modalites <- function(messages, langue) {
  #message<- data$Contenu.du.message
  choix_modalites <- c()
  for (i in 1:length(messages)) {
    if (langue == "fr" && grepl("Tapez", messages[i])) {
      modalite <- messages[i]
      choix <- messages[i + 1]
      if(grepl("Votre choix n'est pas valide", messages[i+2])){
        choix_modalites <- c(choix_modalites, "choix non valide")
      } else if (!is.na(as.numeric(choix))) {
        chaine_choix <- paste("Tapez", choix, sep = " ")
        chaine_recherche <- paste(chaine_choix, ".*")
        sous_chaine <- regmatches(modalite, regexpr(chaine_recherche, modalite))
        resultat <- regmatches(sous_chaine, regexpr(paste0(chaine_choix, "\\s+(.*?)\\s*(?:</br>|$)"), sous_chaine, perl = TRUE))
        resultat <- gsub("^Tapez\\s+\\d+\\s+|\\s*</br>", "", resultat)
        if (grepl("Oui", resultat)) {
          resultat <- "autre préoccupation"
        } else if (grepl("Non", resultat)) {
          resultat <- "aucune autre préoccupation"
        }
        choix_modalites <- c(choix_modalites, paste(choix, ": ", resultat, sep = ""))
      } else {
        if (grepl("Oui", choix)) {
          choix <- "autre préoccupation"
        } else if (grepl("Non", choix)) {
          choix <- "aucune autre préoccupation"
        }
        choix_modalites <- c(choix_modalites, choix)
      }
    } else if (langue == "en" && grepl("Press", messages[i])) {
      modalite <- messages[i]
      choix <- messages[i + 1]
      if(grepl("Your choice is not", messages[i+2])){
        choix_modalites <- c(choix_modalites, "choice not valid")
      } else if (!is.na(as.numeric(choix))) {
        chaine_choix <- paste("Press", choix, sep = " ")
        chaine_recherche <- paste(chaine_choix, ".*")
        sous_chaine <- regmatches(modalite, regexpr(chaine_recherche, modalite))
        resultat <- regmatches(sous_chaine, regexpr(paste0(chaine_choix, "\\s+(.*?)\\s*(?:</br>|$)"), sous_chaine, perl = TRUE))
        resultat <- gsub("^Press\\s+\\d+\\s+|\\s*</br>", "", resultat)
        if (grepl("Yes", resultat)) {
          resultat <- "other issues"
        } else if (grepl("No", resultat)) {
          resultat <- "no other issues"
        }
        choix_modalites <- c(choix_modalites, paste(choix, ": ", resultat, sep = ""))
      } else {
        if (grepl("Yes", choix)) {
          choix <- "other issues"
        } else if (grepl("No", choix)) {
          choix <- "no other issues"
        }
        choix_modalites <- c(choix_modalites, choix)
      }
    }
  }
  return(list(choix_modalites))
}



# Fonction pour traiter les données en fonction de la langue
processData <- function(langue, data) {
  if (langue == "fr") {
    # Extraire les informations personnelles
    summary_messages <- data[str_detect(data$Contenu.du.message, "Récapitulatif de vos informations personnelles"), ]
    summary_messages <- summary_messages %>%
      mutate(Personal_Info = map(Contenu.du.message, extract_personal_info, langue = "fr")) %>%
      bind_cols(bind_rows(.$Personal_Info)) %>%
      select(id, Nom, Prenom, Numero_de_compte, SARA_subscriber_number, NIC_number)

    # Extraire les choix du client avec leurs modalités
    choix_modalites <- extract_choix_modalites(data$Contenu.du.message, langue)

    # Créer le dataframe final
    unique_df <- data.frame(summary_messages[1,], Call_Number = unique(na.omit(data$Numéro.de.téléphone)))
    unique_df$Scenario_Chatbot <- choix_modalites
    agent <- unique(data$Nom.agent)
    selected_agent <- agent[!(agent %in% c(NA, "chatbot", " "))]
    if (length(selected_agent) == 0) {
      unique_df$Agent <- ""
    } else {
      unique_df$Agent <- selected_agent
    }
    unique_df$Start_time_discusion <- min(data$Date.et.heure)
    unique_df$End_time_discusion <- max(data$Date.et.heure)

    chatbot_data <- data %>%
      filter(Nom.agent == "chatbot")
    unique_df$Start_time_chatbot <- min(chatbot_data$Date.et.heure)
    unique_df$End_time_chatbot <- max(chatbot_data$Date.et.heure)

    agent_data <- data %>%
      filter(!is.na(Nom.agent), Nom.agent != "chatbot")
    unique_df$Start_time_agent <- min(agent_data$Date.et.heure)
    unique_df$End_time_agent <- max(agent_data$Date.et.heure)

    # Trouver l'index de la première occurrence de "Veuillez décrire avec précision votre préoccupation"
    index <- grep("Veuillez décrire avec précision votre préoccupation", data$Contenu.du.message)
    if (length(index)==0){
      unique_df$customer_agent_discussion <- list(c())
    } else {
      # Sélectionner les éléments du vecteur jusqu'à cette position
      df <- data[index[length(index)] + 1:length(data$Contenu.du.message),]
      key <- c()
      value <- c()
      for (i in 1:nrow(df)){
        if (!is.na(df$Contenu.du.message[i])){
          key[i] <- ifelse(is.na(df$Nom.agent[i]),"client", "agent")
          value[i] <- df$Contenu.du.message[i]
        }
      }
      unique_df$customer_agent_discussion <- list(data.frame(key,value))

    }
    # Afficher un message et retourner le dataframe final
    print("Données en français traitées")
    return(unique_df)
  } else if (langue == "en") {
    # Extraire les informations personnelles
    summary_messages <- data[str_detect(data$Contenu.du.message, "Summary of your personal informations"), ]
    summary_messages <- summary_messages %>%
      mutate(Personal_Info = map(Contenu.du.message, extract_personal_info, langue = "en")) %>%
      bind_cols(bind_rows(.$Personal_Info)) %>%
      select(id, Nom, Prenom, Numero_de_compte, SARA_subscriber_number, NIC_number)

    # Extraire les choix du client avec leurs modalités
    choix_modalites <- extract_choix_modalites(data$Contenu.du.message, langue)

    # Créer le dataframe final
    unique_df <- data.frame(summary_messages[1,], Call_Number = unique(na.omit(data$Numéro.de.téléphone)))
    unique_df$Scenario_Chatbot <- choix_modalites
    agent <- unique(data$Nom.agent)
    selected_agent <- agent[!(agent %in% c(NA, "chatbot", " "))]

    if (length(selected_agent) == 0) {
      unique_df$Agent <- ""
    } else {
      unique_df$Agent <- selected_agent
    }
    unique_df$Start_time_discusion <- min(data$Date.et.heure)
    unique_df$End_time_discusion <- max(data$Date.et.heure)

    chatbot_data <- data %>%
      filter(Nom.agent == "chatbot")
    unique_df$Start_time_chatbot <- min(chatbot_data$Date.et.heure)
    unique_df$End_time_chatbot <- max(chatbot_data$Date.et.heure)

    agent_data <- data %>%
      filter(!is.na(Nom.agent), Nom.agent != "chatbot")

    unique_df$Start_time_agent <- min(agent_data$Date.et.heure)
    unique_df$End_time_agent <- max(agent_data$Date.et.heure)

    # Trouver l'index de la première occurrence de "Veuillez décrire avec précision votre préoccupation"
    index <- grep("Please describe your concern in detail", data$Contenu.du.message)
    if (length(index)==0){
      unique_df$customer_agent_discussion <- list(c())
    } else {
      # Sélectionner les éléments du vecteur jusqu'à cette position
      df <- data[index[length(index)] + 1:length(data$Contenu.du.message),]
      key <- c()
      value <- c()
      for (i in 1:nrow(df)){
        if (!is.na(df$Contenu.du.message[i])){
          key[i] <- ifelse(is.na(df$Nom.agent[i]),"client", "agent")
          value[i] <- df$Contenu.du.message[i]
        }
      }
      unique_df$customer_agent_discussion <- list(data.frame(key,value))

    }

    # Afficher un message et retourner le dataframe final
    print("Données en anglais traitées")
    return(unique_df)
  } else {
    return("Langue non reconnue")
  }
}



# Définir la fonction processData avec la gestion d'erreur
processDataWithErrorHandling <- function(langue, data) {
  tryCatch(
    {
      df <- processData(langue, data)
      return(df)
    },
    error = function(e) {
      if (grepl("Column .* doesn't exist.", e$message)) {
        message("Erreur: Les colonnes spécifiées n'existent pas dans les données.")
      } else {
        message("Erreur inattendue: ", e$message)
      }
      return(NULL)
    }
  )
}


# Fonction pour extraire les information de cjaque excel file
extract_info <- function(folder_path) {
  # Obtenir la liste des fichiers dans le dossier
  files <- list.files(folder_path, full.names = TRUE)

  # Parcourir chaque fichier
  for (file in files) {
    # Essayer d'appliquer la fonction extract_message_info à chaque fichier
    # Si une erreur se produit, afficher un message et passer au fichier suivant
    #file <- "C:/Kaeyros/Call_center/data/XLSX_conversationsAFB2023/+6798791672_2023-03-21 153254.txt.xlsx"
    tryCatch({
      data <- read_excel(file)

      # Remplacer les occurrences de "chatbot " par "chatbot" dans la colonne Nom.agent
      data$Nom.agent <- gsub("chatbot ", "chatbot", data$Nom.agent)

      # Identifier la langue du texte
      file_pretrained = system.file("language_identification/lid.176.ftz", package = "fastText")
      dtbl_out <- language_identification(data$Contenu.du.message, file_pretrained)
      langue_count <- table(dtbl_out$iso_lang_1)
      langue <- names(which.max(langue_count))

      # Appeler la fonction processDataWithErrorHandling avec la gestion d'erreur
      df <- processDataWithErrorHandling(langue, data)
      # Construire le chemin pour sauvegarder le fichier XLSX
      output_path <- file.path(output_folder, paste0(langue, "_", basename(file), ".json"))
      # Sauvegarder le dataframe comme fichier JSON
      jsonlite::write_json(df, output_path, na = "null")

    }, error = function(e) {
      message("Une erreur s'est produite lors du traitement du fichier ", basename(file), ": ", conditionMessage(e))
    })
    print(basename(file))
  }
}


# Fonction pour fussionner tous les json file obtenu
merge_non_empty_json_files <- function(input_folder, output_filename) {
  #input_folder <- "C:/Kaeyros/Call_center/data/info_conversationsAFB2023"
  # Obtenez la liste des fichiers JSON dans le dossier d'entrée
  json_files <- list.files(input_folder, pattern = "\\.json$", full.names = TRUE)

  # Initialisez une liste pour stocker les données JSON non vides
  json_data <- list()

  # Parcourez chaque fichier JSON
  for (file in json_files) {
    # Lisez le fichier JSON
    data <- fromJSON(file)

    # Vérifiez si le contenu de data est "Langue non reconnue"
    if (identical(data, "Langue non reconnue")) {
      next  # Passez à la prochaine itération de la boucle
    }

    # Affichez le contenu de data pour débogage


    # Vérifiez si le fichier JSON n'est pas vide
    if (!is.null(data) && length(data) > 0 ) {
      # Convertir la colonne Scenario_Chatbot et customer_agent_discussion en liste
      #data$Scenario_Chatbot <- lapply(data$Scenario_Chatbot, as.list)
      #      data$customer_agent_discussion <- lapply(data$customer_agent_discussion, as.list)
      if(class(data$customer_agent_discussion)=="data.frame"){
        data$customer_agent_discussion <- list(data$customer_agent_discussion)
      }
      if(class(data$Scenario_Chatbot)=="data.frame"){
        data$Scenario_Chatbot <- list(data$Scenario_Chatbot)
      }
      print(file)
      print(length(data))
      # Ajoutez les données à la liste
      json_data <- rbind(json_data, data)
    }
  }

  # Fusionnez toutes les données JSON non vides
  #merged_json <- rbind(json_data)
  #output_filename <- file.path(path_data, "info_data_AFB2023.json")
  # Enregistrez les données fusionnées dans un nouveau fichier JSON
  write_json(json_data, output_filename)
}

