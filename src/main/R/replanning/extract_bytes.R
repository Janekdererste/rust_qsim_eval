# Installiere das stringr Paket, wenn es noch nicht installiert ist
# install.packages("stringr")

library(stringr)
library(rlist)

partition_count <- 256

# Pfadeinstellungen anpassen
ordner_pfad <- paste0("./assets/hlrn-all/update-no-replan/size-", partition_count)  # Passe dies entsprechend deinem tatsächlichen Pfad an
datei_name <- "log_process_0.txt"

# Vollständiger Pfad zur Datei
datei_pfad <- file.path(ordner_pfad, datei_name)

# Liste für Index und Größe initialisieren
ergebnisse <- list()

# Datei einlesen
zeilen <- readLines(datei_pfad)

# Muster für die Suche
muster <- "Received travel times message with (\\d+) bytes."

# Suche und Extraktion
treffer <- str_match_all(zeilen, muster)

# Überprüfe, ob es Treffer gibt
if (length(treffer) > 0) {
  # Daten in Liste speichern
  for (i in seq_len(length(treffer))) {
    if(nrow(treffer[[i]]) > 0) {
      ergebnisse <- append(ergebnisse, list(c(update_step = (length(ergebnisse))%/%2, size = as.numeric(treffer[[i]][1, 2]))))
    }
  }
}

library(ggplot2)

update_step <- sapply(ergebnisse, function(x) x["update_step"])
size <- sapply(ergebnisse, function(x) x["size"])

data <- data.frame(update_step = update_step, size = size)

first <- data %>% slice(which(row_number() %% 2 == 0))
ggplot(first, aes(x = update_step, y = size/1000)) +
  geom_point(colour= "blue") +
  labs(x = "Update Step", y = "Size in KByte") +
  ggtitle(paste0("Overall Message Size by Update Step For ", partition_count, " Partitions"))