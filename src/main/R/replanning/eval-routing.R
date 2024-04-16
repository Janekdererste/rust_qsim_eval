library(tidyverse)
library(scales)

source("./src/main/R/tracing.R")
source("./src/main/R/colors.R")
source("./src/main/R/replanning/routing_utils.R")
source("./src/main/R/read_tracing_files.R")

data <- load_rust_tracing_data("./assets/do-replan-64-act-end")

# merge target and func_name colums to func with ::
data <- data %>%
  mutate(func = paste(target, func_name, sep = "::"))

replaning_per_cores <- data %>%
  filter(func == REPLAN_MAIN_KEY) %>%
  group_by(size, rank) %>%
  summarize(count = n(), duration=mean(duration))

print("overall mean is :" )
print(mean(replaning_per_cores$duration))

# plot the number of replanning per core
ggplot(replaning_per_cores, aes(x = count)) +
  geom_histogram(binwidth = 200) +
  labs(title = "Number of Replanning per Core",
       x = "Routing calls",
       y = "Number of Processes") +
  # scale_fill_manual(name = "Core", values = c("#bae4bc", "#7bccc4", "#0868ac")) +
  theme_minimal()+
  ggplot2::theme(
    axis.title.x = element_text(size = 14),  # Schriftgröße der x-Achsenbeschriftung ändern
    axis.title.y = element_text(size = 14),  # Schriftgröße der y-Achsenbeschriftung ändern
    axis.text.x = element_text(size = 14)   # Schriftgröße der x-Achsentickbeschriftungen ändern
  )

# group data by size, rank, func and sim_time. Calculate the sum of the duration for each group
group_func_sim_time <- data %>%
  group_by(rank, func, sim_time) %>%
  summarize(duration = sum(duration))

# filter data by func = REPLAN_MAIN_KEY. plot data as heatmap with time on x-axis, rank on y-axis and color as duration
ggplot(group_func_sim_time %>% filter(func == REPLAN_MAIN_KEY), aes(x = sim_time, y = rank, fill = duration)) +
  geom_tile() +
  labs(title = "Replanning Duration per Rank",
       x = "Simulation Time",
       y = "Rank") +
  scale_fill_viridis_c() +
  theme_minimal()+
  ggplot2::theme(
    axis.title.x = element_text(size = 14),  # Schriftgröße der x-Achsenbeschriftung ändern
    axis.title.y = element_text(size = 14),  # Schriftgröße der y-Achsenbeschriftung ändern
    axis.text.x = element_text(size = 14),   # Schriftgröße der x-Achsentickbeschriftungen ändern
    axis.text.y = element_text(size = 14)   # Schriftgröße der y-Achsentickbeschriftungen ändern
  )

# filter data by func = WAKEUP plot data as heatmap with time on x-axis, rank on y-axis and color as duration
value <- group_func_sim_time %>%
  filter(func == SEND_RECEIVE_KEY) %>%
  filter(sim_time > 10 & sim_time < 1000)

ggplot(value, aes(x = sim_time, y = rank, fill = duration)) +
  geom_tile() +
  labs(title = "Wakeup Duration per Rank",
       x = "Simulation Time",
       y = "Rank") +
  scale_fill_viridis_c() +
  theme_minimal()+
  ggplot2::theme(
    axis.title.x = element_text(size = 14),  # Schriftgröße der x-Achsenbeschriftung ändern
    axis.title.y = element_text(size = 14),  # Schriftgröße der y-Achsenbeschriftung ändern
    axis.text.x = element_text(size = 14),   # Schriftgröße der x-Achsentickbeschriftungen ändern
    axis.text.y = element_text(size = 14)   # Schriftgröße der y-Achsentickbeschriftungen ändern
  )

width <- 30

data_all_ranks <- data %>%
  mutate(
    time_bin = cut_width(sim_time, width = width, boundary = 0, closed = "left"),
    bin_start = floor(sim_time / width) * width
  ) %>%
  group_by(func, bin_start) %>%
  summarize(max_duration = max(duration), diff_duration = max(duration) - min(duration), func_name = first(func_name), max_duration_rank = rank[which.max(duration)])

# plot data_all_ranks as scatter plot. x-axis is sim_time, y-axis is max_duration and color is func. Only plot data with func = REPLAN_MAIN_KEY or func = SEND_RECEIVE_KEY
ggplot(data_all_ranks %>% filter(func == WAKEUP_KEY | func == RECEIVE_KEY), aes(x = bin_start, y = diff_duration/1e6, color = func_name)) +
  geom_point(alpha=0.5) +
  labs(title = "Diff between max. and min. duration per simulation time bin (30s)",
       x = "Simulation Time",
       y = "Max Duration in ms") +
  scale_color_manual(values = neon()) +
  theme_minimal()+
  ggplot2::theme(
    axis.title.x = element_text(size = 14),  # Schriftgröße der x-Achsenbeschriftung ändern
    axis.title.y = element_text(size = 14),  # Schriftgröße der y-Achsenbeschriftung ändern
    axis.text.x = element_text(size = 14),   # Schriftgröße der x-Achsentickbeschriftungen ändern
    axis.text.y = element_text(size = 14)   # Schriftgröße der y-Achsentickbeschriftungen ändern
  )

# plot histrogram of max_duration_rank with bars for func = WAKEUP_KEY and bars for func = RECEIVE_KEY
ggplot(data_all_ranks %>% filter(func == WAKEUP_KEY | func == RECEIVE_KEY), aes(x = max_duration_rank, fill = func_name)) +
  geom_bar(position = "dodge") +
  labs(title = "Rank with max duration per simulation time bin (30s)",
       x = "Rank",
       y = "Count") +
  scale_fill_manual(values = neon()) +
  theme_minimal()+
  ggplot2::theme(
    axis.title.x = element_text(size = 14),  # Schriftgröße der x-Achsenbeschriftung ändern
    axis.title.y = element_text(size = 14),  # Schriftgröße der y-Achsenbeschriftung ändern
    axis.text.x = element_text(size = 14),   # Schriftgröße der x-Achsentickbeschriftungen ändern
    axis.text.y = element_text(size = 14)   # Schriftgröße der y-Achsentickbeschriftungen ändern
  )


# ===================================
on_load <- function(data) {
  transformed <- data %>%
    filter(bin_start > 0) %>%
    filter(func == WAKEUP_KEY | func == RECEIVE_KEY) %>%
    mutate(func_name = sub(".*::", "", func)) %>%
    mutate(sim_time = bin_start) %>%
    mutate(duration = max_dur) %>%
    mutate(func = func_name) %>%
    select(size, sim_time, rank, func, duration)

  return(transformed)
}

all_data <- read_binary_tracing_files(c("./assets/instrument-wr/size-2",
                                        "./assets/instrument-wr/size-8",
                                        "./assets/instrument-wr/size-32",
                                        "./assets/instrument-wr/size-128",
                                        "./assets/instrument-wr/size-512",
                                        "./assets/instrument-wr/size-2048"), on_load = on_load)

grouped_all_data <- all_data %>%
  group_by(size, func, sim_time) %>%
  summarize(max_dur = max(duration), diff_dur = max(duration) - min(duration), func = first(func))

base_breaks <- function(n = 8){
  function(x) {
    axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
  }
}

p <- ggplot(grouped_all_data, aes(x = sim_time, y = max_dur/1e6, color = func)) +
  geom_point(alpha=0.3) +
  facet_wrap(~size) +
  scale_y_log10(labels = label_comma()) +
  labs(title = "Max. Duration per Simulation Time Bin (30s)",
       x = "Simulation Time",
       y = "Max. Duration in ms",
       color = "Function") +
  scale_color_manual(values = neon(), labels = c("wait", "routing")) +
  theme_minimal()+
  ggplot2::theme(
    axis.title.x = element_text(size = 14),  # Schriftgröße der x-Achsenbeschriftung ändern
    axis.title.y = element_text(size = 14),  # Schriftgröße der y-Achsenbeschriftung ändern
    axis.text.x = element_text(size = 14),   # Schriftgröße der x-Achsentickbeschriftungen ändern
    axis.text.y = element_text(size = 14),   # Schriftgröße der y-Achsentickbeschriftungen ändern
    legend.position = "bottom",
    strip.background = element_rect(fill = "grey"),
    strip.text = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14)
  )
p
ggsave("wait-wakeup-runtimes.png", plot = p, device = "png", width = 297, height = 210, units = "mm")


# ===================================

replan_data256 <- read_rds("./assets/merged_data.rds")

replan_data1 <- read_csv("./assets/instrument_process_0.csv") %>%
  filter(func_name =="replan_main")

replan_data256_aggregate <- replan_data256 %>%
  group_by(sim_time, rank) %>%
  summarize(n = n(), max_dur = max(duration)) %>%
  group_by(sim_time) %>%
  summarize(max = max(n), min = min(n), sd= sd(n), mean = mean(n), n= n(), max_dur = max(max_dur)) %>%
  filter(n < 256)

width <- 30
value2 <- replan_data256_aggregate %>%
  mutate(
    time_bin = cut_width(sim_time, width = width, boundary = 0, closed = "left"),
    bin_start = floor(sim_time / width) * width
  ) %>%
  group_by(bin_start) %>%
  summarize(max_dur = max(max_dur))

p <- ggplot(value2, aes(x = bin_start, y = max_dur/1e6)) +
  geom_point() +
  labs(title = "Max. Duration of Replanning Call",
       x = "Simulation Time",
       y = "Max. Duration in ms") +
  theme_minimal()+
  ggplot2::theme(
    axis.title.x = element_text(size = 14),  # Schriftgröße der x-Achsenbeschriftung ändern
    axis.title.y = element_text(size = 14),  # Schriftgröße der y-Achsenbeschriftung ändern
    axis.text.x = element_text(size = 14),   # Schriftgröße der x-Achsentickbeschriftungen ändern
    axis.text.y = element_text(size = 14)   # Schriftgröße der y-Achsentickbeschriftungen ändern
  )
p
ggsave("replanning-calls-258.png", plot = p, device = "png", width = 297, height = 210, units = "mm")

p <- ggplot(replan_data256_aggregate) +
  geom_point(aes(x = sim_time, y = n, colour = "Processes Doing Replanning"), alpha = 0.4) +
  geom_point(aes(x = sim_time, y = max, colour = "Max. Replanning Calls Per Process"), alpha = 0.4) +
  labs(title = "Processes Performing Replanning Calls per Simulation Time",
       x = "Simulation Time",
       y = "Number",
       colour = "Data") +
  scale_color_manual(values = neon()) +
  theme_minimal()+
  ggplot2::theme(
    axis.title.x = element_text(size = 14),  # Schriftgröße der x-Achsenbeschriftung ändern
    axis.title.y = element_text(size = 14),  # Schriftgröße der y-Achsenbeschriftung ändern
    axis.text.x = element_text(size = 14),   # Schriftgröße der x-Achsentickbeschriftungen ändern
    axis.text.y = element_text(size = 14),   # Schriftgröße der y-Achsentickbeschriftungen ändern
    legend.position = "bottom",
    strip.background = element_rect(fill = "grey"),
    strip.text = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14)
  )
p
ggsave("replanning-calls-258-processes.png", plot = p, device = "png", width = 297, height = 210, units = "mm")

