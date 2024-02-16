# Installiere ggplot2, falls es noch nicht installiert ist
# install.packages("ggplot2")

# Lade das ggplot2-Paket
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)

# Lese die CSV-Datei ein
data <- read.csv("../../../../assets/act_starts.csv")

# Definiere die Blockgröße
block_size <- 900

# Erstelle eine neue Spalte "time_block", die die Zeit in Blöcke unterteilt
data$time_block <- cut(data$time, breaks = seq(0, max(data$time), by = block_size), labels = FALSE)

grouped_data <- data %>%
  group_by(time_block, partition) %>%
  summarise(total_count = sum(count))

# Erstelle die Heatmap
heatmap_plot <- ggplot(grouped_data, aes(x = time_block, y = partition, fill = total_count)) +
  geom_tile() +
  scale_fill_viridis(discrete=TRUE) +
  labs(title = "Heatmap der summierten Count-Werte nach Zeitblöcken und Partitionen",
       x = "Time Block",
       y = "Partition Block",
       fill = "Total Count")

print(heatmap_plot)