# Installieren Sie zuerst das Paket "tidyverse", wenn Sie es noch nicht installiert haben
# install.packages("tidyverse")

# Laden Sie das Paket "tidyverse" und "stringr"
library(tidyverse)
library(stringr)
library(purrr)
PATTERN <- "Partition #([0-9]+) network has: ([0-9]+) nodes and ([0-9]+) links"

extract_numbers_from_line <- function(line, process_count) {
  matches <- str_match(line, PATTERN)
  if (!is.na(matches[1, 2])) {
    prozess_nummer <- process_count
    anzahl_nodes <- as.integer(matches[1, 3])
    anzahl_links <- as.integer(matches[1, 4])

    return (c(process = prozess_nummer, nodes = anzahl_nodes, links = anzahl_links))
  } else {
    return (NULL)
  }
}

extract_links_per_process <- function(file_path) {
  log_data <- readLines(file_path)
  # filter log_data so that it only contains lines that match the pattern
  log_data <- log_data[grep(PATTERN, log_data)]
  callback <- partial(extract_numbers_from_line, process_count = length(log_data))
  numbers <- map(log_data, callback)
  numbers <- numbers[!sapply(numbers, is.null)]

  df <- do.call(rbind, numbers)

  return (df)
}

get_entry_with_number_of_processes <- function(processes, process_count) {
  return (Filter(function(x) nrow(x) == process_count, processes)[[1]])
}

log_folder <- "./assets/hlrn"
log_files <- list.files(log_folder, pattern = "^[0-9]+-.+\\.log$", full.names = TRUE)
results <- map(log_files, extract_links_per_process)

print(get_entry_with_number_of_processes(results, 64))

data <- do.call(rbind, lapply(results, as.data.frame))
colnames(data) <- c("process", "nodes", "links")

data <- data %>%
  group_by(process) %>%
  summarise(rel_delta = (max(links) - min(links))/mean(links), delta = max(links) - min(links), mean = mean(links))

# Erstelle den Plot mit ggplot2
ggplot(data, aes(x = factor(process), y = rel_delta)) +
  geom_point(color = "blue") +
  labs(title = "Relatives Delta auf Anzahl der Prozesse", x = "Anzahl der Processes", y = "Relative Delta #Links")

ggplot(data, aes(x = factor(process), y = delta)) +
  geom_point(color = "red") +
  labs(title = "Absolutes Delta auf Anzahl der Prozesse", x = "Anzahl der Processes", y = "Delta #Links")

ggplot(data, aes(x = factor(process), y = mean)) +
  geom_point(color = "orange") +
  geom_text(aes(label = sprintf("%.0f", mean)), vjust = -0.5, size = 3) +
  scale_y_log10() +
  labs(title = "Durchschnitt auf Anzahl der Prozesse", x = "Anzahl der Processes", y = "Mean #Links")