#tidyverse https://www.tidyverse.org/ which is the stuff we use to wrangle and plot our data
library(tidyverse)
library(dplyr)
library(tidyr)

source("./src/main/R/colors.R")
source("./src/main/R/parsing.R")
source("./src/main/R/tracing.R")
source("./src/main/R/replanning/routing_utils.R")

all_traces <- load_rust_tracing_data("./assets/hlrn-all/update-no-replan", num_cores = 16)

overall_runtimes <- all_traces %>%
  filter(sim_time != 0) %>% # remove the first entry
  group_by(size) %>%
  summarise(
    Collecting = mean(tail(na.omit(duration[func == COLLECT_KEY], -1))) / 1e6,
    Inserting = mean(tail(na.omit(duration[func == INSERTION_KEY], -1))) / 1e6,
    Communicating = mean(tail(na.omit(duration[func == COMMUNICATION_KEY], -2))) / 1e6,
    Handling = mean(tail(na.omit(duration[func == HANDLE_KEY], -2))) / 1e6,
  )

data_long <- gather(overall_runtimes, key = "duration_type", value = "duration_value", -size)

data_long$size <- as.factor(data_long$size)

gewuenschte_reihenfolge <- c("Handling", "Communicating", "Inserting", "Collecting")  # Füge die tatsächlichen Phasen hinzu

# Setze die Faktorstufen in die gewünschte Reihenfolge
data_long$duration_type <- factor(data_long$duration_type, levels = gewuenschte_reihenfolge)

# Gestapeltes Balkendiagramm erstellen
ggplot(data_long, aes(x = size, y = duration_value, fill = duration_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Durations of Udpate Process",
       x = "#Partitions",
       y = "Duration in ms") +
  scale_fill_manual(name = "Phases", values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3")) +
  theme_minimal()

ggsave("overall-runtime.pdf")

overall_runtimes <- overall_runtimes %>%
  mutate(TotalDuration = Collecting + Inserting + Communicating + Handling)

overall_runtimes <- overall_runtimes %>%
  mutate(Speedup = overall_runtimes$TotalDuration[1] / TotalDuration)

# ganzes speedup Diagramm
ggplot(overall_runtimes, aes(x = size, y = Speedup)) +
  geom_line() +
  scale_x_continuous(trans = 'log2', breaks = overall_runtimes$size) +
  scale_y_continuous(trans = 'log2') +
  labs(title = "Overall Speedup Plot",
       x = "Size",
       y = "Speedup") +
  theme_minimal()
ggsave("overall-speedup.pdf")

# speedup diagramm für collecting
ggplot(overall_runtimes, aes(x = size)) +
  geom_line(aes(y = overall_runtimes$Collecting[1] / Collecting, colour = "Collecting")) +
  geom_line(data = overall_runtimes, aes(x = size, y = overall_runtimes$Inserting[1] / Inserting, colour = "Inserting")) +
  scale_x_continuous(trans = 'log2', breaks = overall_runtimes$size) +
  scale_y_continuous(trans = 'log2') +
  scale_color_manual(values = c("Collecting" = red(), "Inserting" = blue()), name = "Phase") +
  labs(title = "Speedup Plot for Collecting and Inserting",
       x = "Size",
       y = "Speedup") +
  theme_minimal()
ggsave("collecting-inserting-speedup.pdf")

# barplot für communicating
communication_runtimes <- all_traces %>%
  group_by(size) %>%
  summarise(
    Sync = mean(tail(na.omit(duration[func == GATHER_LENGTHS_KEY], -2))) / 1e6,
    "Gather Data" = mean(tail(na.omit(duration[func == GATHER_TT_KEY], -2))) / 1e6,
    Deserialization = mean(tail(na.omit(duration[func == DESERIALIZE_KEY], -2))) / 1e6,
  )

ggplot(overall_runtimes, aes(x = factor(size), y = Communicating)) +
  geom_bar(stat = "identity", fill = blue()) +
  labs(title = "Mean Communicating Durations",
       x = "Size",
       y = "Duration in ms") +
  theme_minimal()
ggsave("communication-bars.pdf")

comm_data_long <- gather(communication_runtimes, key = "duration_type", value = "duration_value", -size)

comm_data_long$size <- as.factor(comm_data_long$size)

ggplot(comm_data_long, aes(x = size, y = duration_value, fill = duration_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Durations of Comunication Per Mode",
       x = "#Partitions",
       y = "Duration in ms") +
  scale_fill_manual(name = "Phases", values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3")) +
  theme_minimal()
ggsave("phases-communication.pdf")

# barplot für handling
ggplot(overall_runtimes, aes(x = factor(size), y = Handling)) +
  geom_bar(stat = "identity", fill = blue()) +
  labs(title = "Mean Handling Durations",
       x = "Size",
       y = "Duration in ms") +
  theme_minimal()
ggsave("handling-bars.pdf")