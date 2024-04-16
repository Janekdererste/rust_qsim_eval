#tidyverse https://www.tidyverse.org/ which is the stuff we use to wrangle and plot our data
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggpattern)

source("./src/main/R/colors.R")
source("./src/main/R/parsing.R")
source("./src/main/R/tracing.R")
source("./src/main/R/replanning/routing_utils.R")
source("./src/main/R/colors.R")

all_traces <- read_binary_tracing_files("./assets/instrument-router-updates-wr")
all_traces_nr <- read_binary_tracing_files("./assets/instrument-router-update-nr")

preprocessing <- all_traces %>%
  filter(func == COLLECT_KEY | func == INSERTION_KEY) %>%
  filter(sim_time != 0) %>%
  group_by(size, sim_time, rank) %>%
  summarise(duration = sum(duration)) %>%
  group_by(size) %>%
  summarise(mean_duration = mean(duration), median_duration = median(duration), func = "3preprocessing", type = "1wr")

communicate_all <- all_traces %>%
  filter(func == COMMUNICATION_ALL_KEY) %>%
  filter(sim_time != 0) %>%
  group_by(size, sim_time, rank) %>%
  summarise(duration = sum(duration)) %>%
  group_by(size) %>%
  summarise(mean_duration = mean(duration), median_duration = median(duration), func = "2communication", type = "1wr")

handle_all <- all_traces %>%
  filter(func == HANDLE_KEY) %>%
  filter(sim_time != 0) %>%
  group_by(size, sim_time, rank) %>%
  summarise(duration = sum(duration)) %>%
  group_by(size) %>%
  summarise(mean_duration = mean(duration), median_duration = median(duration), func = "1postprocessing", type = "1wr")

preprocessing_nr <- all_traces_nr %>%
  filter(func == COLLECT_KEY | func == INSERTION_KEY) %>%
  filter(sim_time != 0) %>%
  group_by(size, sim_time, rank) %>%
  summarise(duration = sum(duration)) %>%
  group_by(size) %>%
  summarise(mean_duration = mean(duration), median_duration = median(duration), func = "3preprocessing", type = "2nr")

communicate_all_nr <- all_traces_nr %>%
  filter(func == COMMUNICATION_ALL_KEY) %>%
  filter(sim_time != 0) %>%
  group_by(size, sim_time, rank) %>%
  summarise(duration = sum(duration)) %>%
  group_by(size) %>%
  summarise(mean_duration = mean(duration), median_duration = median(duration), func = "2communication", type = "2nr")

handle_all_nr <- all_traces_nr %>%
  filter(func == HANDLE_KEY) %>%
  filter(sim_time != 0) %>%
  group_by(size, sim_time, rank) %>%
  summarise(duration = sum(duration)) %>%
  group_by(size) %>%
  summarise(mean_duration = mean(duration), median_duration = median(duration), func = "1postprocessing", type = "2nr")

stacked_traces <- bind_rows(preprocessing, communicate_all, handle_all, preprocessing_nr, communicate_all_nr, handle_all_nr)

p <- ggplot(stacked_traces) +
  geom_bar(aes(x = type, y = median_duration/1e6, fill = func),
           data = subset(stacked_traces, type == "1wr"),
           stat = "identity", position = "stack") +
  geom_bar(aes(x = type, y = median_duration/1e6, fill = func),
           data = subset(stacked_traces, type == "2nr"),
           stat = "identity", position = "stack", alpha = 0.5) +
  facet_grid(~ size, switch = "x") +
  labs(title = "Median Durations of Update Phases",
       x = "Number of Processes",
       y = "Duration in ms") +
  scale_fill_manual(name = "Phases", values = neon(),
                    labels = c("postprocessing", "communication", "preprocesing")) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 14),
    plot.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.position = "bottom"
  )
p
ggsave("router-update-overall.png", plot = p, device = "png", width = 297, height = 210, units = "mm")


# =================================


cc <- all_traces %>%
  filter(func == COMMUNICATION_KEY & sim_time != 0) %>%
  mutate(Index = rep(c(0, 1), length.out = nrow(.))) %>%
  filter(Index == 0)

hh <- all_traces %>%
  filter(func == HANDLE_KEY & sim_time != 0) %>%
  mutate(Index = rep(c(0, 1), length.out = nrow(.))) %>%
  filter(Index == 0)

# merge cc and hh and comm_traces without communication and handle
tt <- bind_rows(cc, hh, all_traces %>%
  filter(func != COMMUNICATION_KEY, func != HANDLE_KEY))

overall_runtimes <- all_traces %>%
  filter(sim_time != 0) %>% # remove the first entry
  group_by(size) %>%
  summarise(
    PreProcessingI = mean(tail(na.omit(duration[func == COLLECT_KEY], -1))) / 1e6,
    PreProcessingII = mean(tail(na.omit(duration[func == INSERTION_KEY], -1))) / 1e6,
    communication = mean(tail(na.omit(duration[func == COMMUNICATION_ALL_KEY], -1))) / 1e6,
    postprocessing = 2 * mean(tail(na.omit(duration[func == HANDLE_KEY], -1))) / 1e6,
  ) %>%
  mutate(PreProcessing = PreProcessingI + PreProcessingII) %>%
  select(-PreProcessingI, -PreProcessingII)

data_long <- gather(overall_runtimes, key = "duration_type", value = "duration_value", -size)

data_long$size <- as.factor(data_long$size)

gewuenschte_reihenfolge <- c("postprocessing", "communication", "preprocessing")  # Füge die tatsächlichen Phasen hinzu

# Setze die Faktorstufen in die gewünschte Reihenfolge
data_long$duration_type <- factor(data_long$duration_type, levels = gewuenschte_reihenfolge)

# Gestapeltes Balkendiagramm erstellen
p <- ggplot(data_long, aes(x = size, y = duration_value, fill = duration_type)) +
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
ggsave("router-update-overall.png", plot = p, device = "png", width = 297, height = 210, units = "mm")

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

gather_lengths <- all_traces %>%
  filter(func == GATHER_LENGTHS_KEY & sim_time != 0) %>%
  group_by(func, size, sim_time, rank) %>%
  summarise(duration = sum(duration)) %>%
  group_by(func, size) %>%
  summarise(mean_duration = mean(duration))

x4 <- all_traces %>%
  filter(size==4096) %>%
    filter(func == COMMUNICATION_ALL_KEY & sim_time != 0) %>%
    group_by(func, size, sim_time, rank) %>%
  summarise(sum_duration = sum(duration), min_dur = min(duration), max_dur = max(duration), n = n())

gather_tt <- all_traces %>%
  filter(func == GATHER_TT_KEY & sim_time != 0) %>%
  group_by(func, size, sim_time, rank) %>%
  summarise(duration = sum(duration)) %>%
  group_by(func, size) %>%
  summarise(mean_duration = mean(duration))

deserial <- all_traces %>%
  filter(func == DESERIALIZE_KEY & sim_time != 0) %>%
  mutate(index = (row_number()-1)%/%2) %>%
  group_by(func, size, index, rank) %>%
  summarise(duration = sum(duration)) %>%
  group_by(func, size) %>%
  summarise(mean_duration = mean(duration))

cc_traces <- bind_rows(gather_lengths, gather_tt, deserial)

ggplot(cc_traces, aes(x = as.factor(size), y = mean_duration/1e6, fill = func)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Mean Durations of Comunication",
       x = "Number of Processes",
       y = "Duration in ms") +
  scale_fill_manual(name = "Phases", values = neon(), labels = c("deserialization", "sync", "data exchange")) +
  theme_minimal() +
  ggplot2::theme(
    axis.title.x = element_text(size = 14),  # Schriftgröße der x-Achsenbeschriftung ändern
    axis.title.y = element_text(size = 14),  # Schriftgröße der y-Achsenbeschriftung ändern
    axis.text.x = element_text(size = 14),   # Schriftgröße der x-Achsentickbeschriftungen ändern
    axis.text.y = element_text(size = 14),   # Schriftgröße der y-Achsentickbeschriftungen ändern
    plot.title = element_text(size = 14) ,    # Schriftgröße des Plot-Titels ändern
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),# Schriftgröße der Legende ändern
    legend.position = "bottom"   # Schriftgröße der Legendenbeschriftungen ändern
  )
ggsave("router-update-communication.png", plot = p, device = "png", width = 297, height = 210, units = "mm")

