# Installiere ggplot2, falls es noch nicht installiert ist
# install.packages("ggplot2")

# Lade das ggplot2-Paket
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)

# Lese die CSV-Datei ein
data <- read.csv("assets/cluster-act/size-128/act_starts.csv")

# Definiere die Blockgröße
block_size <- 900

# Erstelle eine neue Spalte "time_block", die die Zeit in Blöcke unterteilt
data$time_block <- cut(data$time, breaks = seq(0, max(data$time), by = block_size), labels = FALSE)
data$time_block[is.na(data$time_block)] <- 0

grouped_data <- data %>%
  group_by(time_block, partition) %>%
  summarise(total_count = sum(count))

# Erstelle die Heatmap mit time blocks
ggplot(grouped_data, aes(x = time_block, y = factor(partition), fill = total_count)) +
  geom_tile() +
  scale_fill_viridis(discrete=FALSE) +
  labs(title = "Heatmap der summierten Count-Werte nach Zeitblöcken und Partitionen",
       x = "Time Block",
       y = "Partition Block",
       fill = "Total Count")

# data by time == 43200
data_43200 <- data %>%
  filter(time == 43200)

ggplot(data_43200, aes(x = partition, y = count)) +
  geom_point() +
  geom_text(aes(label = sprintf("%.0f", partition)), vjust = -0.5, size = 3) +
  labs(title = "Replans per Partition",
       x = "Partition #",
       y = "# Replans")