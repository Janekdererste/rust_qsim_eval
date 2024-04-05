library(tidyverse)
library(jsonlite)
library(lubridate)
library(hrbrthemes)
library(viridis)
require(scales)
library(dplyr)

source("./src/main/R/colors.R")
source("./src/main/R/parsing.R")
source("./src/main/R/tracing.R")
source("./src/main/R/replanning/routing_utils.R")

comm_traces <- load_rust_tracing_data("assets/hlrn-all/update-do-replan", num_cores = 16)

first_sync <- comm_traces %>%
  filter(size == 1 ) %>%
  filter(func == GATHER_LENGTHS_KEY)
# Plot first sync durations
first_sync <- comm_traces %>%
  filter(func == GATHER_LENGTHS_KEY) %>%
  mutate(index = (row_number()-1) %% (97 * 2)) %>%
  filter(index %% 2 == 0) %>%
  filter(index > 0) %>%
  mutate(sim_time = index/2 * 900) %>%
  mutate(c = rank/size * 1000)

ggplot(first_sync, aes(x = sim_time, y = duration / 1e6, color = factor(c))) +
  geom_point(alpha = 0.4, size = 0.5) +
  facet_wrap(~size) +
  # scale_color_viridis_d(option = "D") +  # Verwenden Sie die Viridis Farbskala für die Farbskala
  # scale_fill_viridis_d(option = "D") +  # Verwenden Sie die Viridis Farbskala für die Füllfarbe
  scale_y_continuous(trans = "log2", breaks = scales::trans_breaks("log2", function(x) 2^x),
                labels = scales::trans_format("log2", scales::math_format(2^.x))) +
  xlab("Simulation Time") +
  ylab("Mean Duration ms") +
  ggtitle("Execution Times of Wait") +
  theme_light() +
  ggplot2::theme(legend.position = "none",
                 axis.title.x = element_text(size = 14),  # Schriftgröße der x-Achsenbeschriftung ändern
                 axis.title.y = element_text(size = 14),  # Schriftgröße der y-Achsenbeschriftung ändern
                 plot.title = element_text(size = 14) ,    # Schriftgröße des Plot-Titels ändern
                 legend.title = element_text(size = 14),    # Schriftgröße der Legende ändern
                 legend.text = element_text(size = 14))

ggsave("wait-runtime-do-replan.pdf")

# plot preprocessing
preprocessing <- comm_traces %>%
  filter(func == COLLECT_KEY | func == INSERTION_KEY)

# Create a vector to store the new values
new_rank <- preprocessing$rank
# Identify the rows corresponding to COLLECT_KEY
collect_key_rows <- which(preprocessing$func == COLLECT_KEY)
# Update the rank for COLLECT_KEY rows
new_rank[collect_key_rows] <- preprocessing$rank[collect_key_rows + 1]
# Update the dataframe
preprocessing$rank <- new_rank

preprocessing_group <- preprocessing %>%
  group_by(sim_time, rank, size) %>%
  summarise(d = sum(duration)) %>%
  ungroup() %>%
  mutate(c = rank/size * 1000)

ggplot(preprocessing_group, aes(x = sim_time, y = d / 1e6, color = factor(c))) +
  geom_point(alpha = 0.4, size = 0.5) +
  facet_wrap(~size) +
  xlab("Simulation Time") +
  ylab("Mean Duration ms") +
  scale_y_continuous(trans = "log2", breaks = scales::trans_breaks("log2", function(x) 2^x),
                     labels = scales::trans_format("log2", scales::math_format(2^.x))) +
  # scale_y_log10()+
  ggtitle("Execution Times of Preprocessing") +
  theme_light() +
  ggplot2::theme(legend.position = "none",
                 axis.title.x = element_text(size = 14),  # Schriftgröße der x-Achsenbeschriftung ändern
                 axis.title.y = element_text(size = 14),  # Schriftgröße der y-Achsenbeschriftung ändern
                 plot.title = element_text(size = 14) ,    # Schriftgröße des Plot-Titels ändern
                 legend.title = element_text(size = 14),    # Schriftgröße der Legende ändern
                 legend.text = element_text(size = 14))
ggsave("preprocessing-runtime-do-replan.pdf")

pre_delta <- preprocessing_group %>%
  group_by(sim_time, size) %>%
  summarise(pre_max = max(d), pre_min = min(d)) %>%
  ungroup() %>%
  mutate(pre_delta = pre_max - pre_min) %>%
  filter(sim_time >0)

sync_delta <- first_sync %>%
  group_by(sim_time, size) %>%
  summarise(sync_max = max(duration), sync_min = min(duration)) %>%
  ungroup() %>%
  mutate(sync_delta = sync_max - sync_min, rel_error = sync_delta / sync_max)

merged <- inner_join(pre_delta, sync_delta, by = c("sim_time", "size"))

merged <- merged %>%
  mutate(delta_diff = pre_delta - sync_delta)

ggplot(merged, aes(x = sim_time, y = delta_diff / 1e6)) +
  geom_line(alpha = 0.4, size = 0.5) +
  facet_wrap(~size) +
  xlab("Simulation Time") +
  ylab("Mean Duration ms") +
  # scale_y_log10()+
  ggtitle("Delta Pre - Delta Wait") +
  theme_light() +
  ggplot2::theme(legend.position = "none",
                 axis.title.x = element_text(size = 14),  # Schriftgröße der x-Achsenbeschriftung ändern
                 axis.title.y = element_text(size = 14),  # Schriftgröße der y-Achsenbeschriftung ändern
                 plot.title = element_text(size = 14) ,    # Schriftgröße des Plot-Titels ändern
                 legend.title = element_text(size = 14),    # Schriftgröße der Legende ändern
                 legend.text = element_text(size = 14))
# ggsave("pre-sync-delta.pdf")

# plot sync on preprocessing
sync_on_pre <- inner_join(preprocessing_group, first_sync, by = c("sim_time", "size", "rank")) %>%
  mutate(total_dur = duration/1e6 + d/1e6) %>%
  filter(size ==2048)

ggplot(sync_on_pre, aes(x = sim_time/900, y = factor(rank) , fill = total_dur)) +
  # geom_point(alpha = 0.4, size = 0.5) +
  geom_tile()+
  # facet_wrap(~size, scales = "free") +
  # scale_color_continuous(type = "viridis") +
  scale_fill_viridis(discrete=FALSE, name = "Preprocessing + Wait Duration in ms", direction = 1) +
  xlab("Update Step") +
  ylab("Rank") +
  # scale_y_log10()+
  ggtitle("Preprocessing + Wait Duration in ms") +
  theme_light() +
  ggplot2::theme(legend.position = "right",
                 axis.title.x = element_text(size = 14),  # Schriftgröße der x-Achsenbeschriftung ändern
                 axis.title.y = element_text(size = 14),  # Schriftgröße der y-Achsenbeschriftung ändern
                 plot.title = element_text(size = 14) ,    # Schriftgröße des Plot-Titels ändern
                 legend.title = element_text(size = 14),    # Schriftgröße der Legende ändern
                 legend.text = element_text(size = 14))+
  scale_y_discrete(breaks = unique(sync_on_pre$rank)[c(TRUE, rep(FALSE, 31))])+
  scale_x_continuous(breaks = seq(0, max(sync_on_pre$sim_time/900), by = 4))
#ggsave("pre+wait.pdf", height = 210, width=400, units = "mm", device = "pdf")

# plot duration on sim time of func = ROUTING_KEY
routing <- comm_traces %>%
  filter(func == ROUTING_KEY) %>%
  group_by(sim_time, size, rank) %>%
  summarise(routing_dur = sum(duration))

ggplot(routing, aes(x = sim_time, y = routing_dur / 1e6)) +
  geom_point(alpha = 0.4, size = 0.5) +
  facet_wrap(~size) +
  xlab("Simulation Time") +
  ylab("Mean Duration ms") +
  theme_light() +
  ggplot2::theme(legend.position = "none",
                 axis.title.x = element_text(size = 14),  # Schriftgröße der x-Achsenbeschriftung ändern
                 axis.title.y = element_text(size = 14),  # Schriftgröße der y-Achsenbeschriftung ändern
                 plot.title = element_text(size = 14) ,    # Schriftgröße des Plot-Titels ändern
                 legend.title = element_text(size = 14),    # Schriftgröße der Legende ändern
                 legend.text = element_text(size = 14))