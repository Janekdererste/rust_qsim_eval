#tidyverse https://www.tidyverse.org/ which is the stuff we use to wrangle and plot our data
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggpattern)

source("./src/main/R/colors.R")
source("./src/main/R/parsing.R")
source("./src/main/R/tracing.R")
source("./src/main/R/replanning/routing_utils.R")

all_traces <- load_rust_tracing_data("./assets/hlrn-all/update-no-replan", num_cores = 16)

cc <- comm_traces %>%
  filter(func == COMMUNICATION_KEY & sim_time != 0) %>%
  mutate(Index = rep(c(0, 1), length.out = nrow(.))) %>%
  filter(Index == 0)

hh <- comm_traces %>%
  filter(func == HANDLE_KEY & sim_time != 0) %>%
  mutate(Index = rep(c(0, 1), length.out = nrow(.))) %>%
  filter(Index == 0)

# merge cc and hh and comm_traces without communication and handle
tt <- bind_rows(cc, hh, comm_traces %>%
  filter(func != COMMUNICATION_KEY, func != HANDLE_KEY))

overall_runtimes <- tt %>%
  filter(sim_time != 0) %>% # remove the first entry
  group_by(size) %>%
  summarise(
    PreProcessingI = mean(tail(na.omit(duration[func == COLLECT_KEY], -1))) / 1e6,
    PreProcessingII = mean(tail(na.omit(duration[func == INSERTION_KEY], -1))) / 1e6,
    Communication = mean(tail(na.omit(duration[func == COMMUNICATION_KEY], -2))) / 1e6,
    PostProcessing = mean(tail(na.omit(duration[func == HANDLE_KEY], -2))) / 1e6,
  ) %>%
  mutate(PreProcessing = PreProcessingI + PreProcessingII) %>%
  select(-PreProcessingI, -PreProcessingII)

data_long <- gather(overall_runtimes, key = "duration_type", value = "duration_value", -size)

data_long$size <- as.factor(data_long$size)

gewuenschte_reihenfolge <- c("PostProcessing", "Communication", "PreProcessing")  # Füge die tatsächlichen Phasen hinzu

# Setze die Faktorstufen in die gewünschte Reihenfolge
data_long$duration_type <- factor(data_long$duration_type, levels = gewuenschte_reihenfolge)

# Gestapeltes Balkendiagramm erstellen
ggplot(data_long, aes(x = size, y = duration_value, fill = duration_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Durations of Udpate Process",
       x = "Number of Processes",
       y = "Duration in ms") +
  scale_fill_manual(name = "Phases", values = c("#bae4bc", "#7bccc4", "#0868ac")) +
  theme_minimal()+
  ggplot2::theme(
    axis.title.x = element_text(size = 14),  # Schriftgröße der x-Achsenbeschriftung ändern
    axis.title.y = element_text(size = 14),  # Schriftgröße der y-Achsenbeschriftung ändern
    axis.text.x = element_text(size = 14),   # Schriftgröße der x-Achsentickbeschriftungen ändern
    axis.text.y = element_text(size = 14),   # Schriftgröße der y-Achsentickbeschriftungen ändern
    plot.title = element_text(size = 14) ,    # Schriftgröße des Plot-Titels ändern
    legend.title = element_text(size = 14),    # Schriftgröße der Legende ändern
    legend.text = element_text(size = 14)     # Schriftgröße der Legendenbeschriftungen ändern
  )

ggsave("overall-runtime-no-replan.pdf")

# speedup diagramm für Aggregation
ggplot(overall_runtimes, aes(x = size)) +
  geom_line(aes(y = overall_runtimes$PreProcessing[1] / PreProcessing, colour = "PreProcessing")) +
  scale_x_continuous(trans = 'log2', breaks = overall_runtimes$size) +
  scale_y_continuous(trans = "log2", breaks = scales::trans_breaks("log2", function(x) 2^x)
                     ) +
  scale_color_manual(values = c("PreProcessing" = red()), name = "Phase") +
  labs(title = "Speedup Plot for PreProcessing",
       x = "Size",
       y = "Speedup") +
  theme_minimal()
ggsave("preprocessing-speedup-no-replan.pdf")

# barplot für Communication

gather_lengths <- comm_traces %>%
  filter(func == GATHER_LENGTHS_KEY & sim_time != 0) %>%
  mutate(Index = rep(c(0, 1), length.out = nrow(.))) %>%
  filter(Index == 0)

gather_tt <- comm_traces %>%
  filter(func == GATHER_TT_KEY & sim_time != 0) %>%
  mutate(Index = rep(c(0, 1), length.out = nrow(.))) %>%
  filter(Index == 0)

deserial <- comm_traces %>%
  filter(func == DESERIALIZE_KEY & sim_time != 0) %>%
  mutate(Index = rep(c(0, 1), length.out = nrow(.))) %>%
  filter(Index == 0)

cc_traces <- bind_rows(gather_lengths, gather_tt,deserial, comm_traces %>%
  filter(func != GATHER_LENGTHS_KEY, func != GATHER_TT_KEY, func != DESERIALIZE_KEY))

communication_runtimes <- cc_traces %>%
  group_by(size) %>%
  summarise(
    Sync = mean(tail(na.omit(duration[func == GATHER_LENGTHS_KEY], -2))) / 1e6,
    "Gather Data" = mean(tail(na.omit(duration[func == GATHER_TT_KEY], -2))) / 1e6,
    Deserialization = mean(tail(na.omit(duration[func == DESERIALIZE_KEY], -2))) / 1e6,
  )

comm_data_long <- gather(communication_runtimes, key = "duration_type", value = "duration_value", -size)

comm_data_long$size <- as.factor(comm_data_long$size)

ggplot(comm_data_long, aes(x = size, y = duration_value, fill = duration_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Durations of Comunication",
       x = "#Partitions",
       y = "Duration in ms") +
  scale_fill_manual(name = "Phases", values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3")) +
  theme_minimal()
ggsave("phases-communication.pdf")
