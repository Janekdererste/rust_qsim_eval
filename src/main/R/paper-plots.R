library(tidyverse)
library(scales)
library(extrafont)
library(patchwork)
source("./src/main/R/read_tracing_files.R")
source("./src/main/R/colors.R")

# Import and load fonts
#extrafont::font_import(prompt = FALSE)
#fonts() this would list all installed fonts
#loadfonts(device = "pdf")

function_names <- c("rust_q_sim::simulation::messaging::communication::communicators::send_msgs",
                    "rust_q_sim::simulation::messaging::communication::communicators::receive_msgs",
                    "rust_q_sim::simulation::messaging::communication::communicators::handle_msgs",
                    "rust_q_sim::simulation::simulation::wakeup",
                    "rust_q_sim::simulation::simulation::terminate_teleportation",
                    "rust_q_sim::simulation::network::sim_network::move_nodes",
                    "rust_q_sim::simulation::network::sim_network::move_links",
                    "rust_q_sim::simulation::messaging::events::finish",
                    "rust_q_sim::simulation::simulation::run")
labels <- c("Send", "Receive", "Handle", "Finish activities", "Teleport", "Move nodes", "Move links", "Finish", "Run")
function_filter <- c("Send", "Receive", "Handle", "Finish activities", "Teleport", "Move nodes", "Move links", "Run")
work_filter <- c("Finish activities", "Teleport", "Move nodes", "Move links")
comm_filter <- c("Send", "Receive", "Handle")
func_to_label <- setNames(labels, function_names)

match_func_labels <- function(func_value) {
  func_to_label[func_value]
}

on_load_tracing <- function(data) {
  data %>%
    mutate(func = sapply(func, match_func_labels)) %>%
    filter(func %in% function_filter) %>%
    mutate(sim_time = bin_start) %>%
    mutate(duration = median_dur) %>%
    select(size, sim_time, rank, func, duration)
}

tracing_10pct <- read_binary_tracing_files(
  "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/scaling-tracing/instrument-10.0",
  on_load = on_load_tracing, parallel = TRUE)

tracing_0pct <- read_binary_tracing_files(
  "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/scaling-tracing/instrument-0.0",
  on_load = on_load_tracing, parallel = TRUE
)

timings_0pct <- read_binary_tracing_files(
  "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/scaling-runtimes/instrument/rvr-0.0pct",
) %>%
  filter(func == "rust_q_sim::simulation::simulation::run") %>%
  group_by(size) %>%
  summarize(run_time = mean(median_dur) / 1e9) %>%
  mutate(rtr = 129600 / run_time) %>%
  mutate(speedup = run_time[size == 1] / run_time) %>%
  mutate(name = "Prototype 0%")

timings_1pct <- read_binary_tracing_files(
  "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/scaling-runtimes/instrument/rvr-1.0pct",
) %>%
  filter(func == "rust_q_sim::simulation::simulation::run") %>%
  group_by(size) %>%
  summarize(run_time = mean(median_dur) / 1e9) %>%
  mutate(rtr = 129600 / run_time) %>%
  mutate(speedup = run_time[size == 1] / run_time) %>%
  mutate(name = "Prototype 1%")
timings_3pct <- read_binary_tracing_files(
  "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/scaling-runtimes/instrument/rvr-3.0pct",
) %>%
  filter(func == "rust_q_sim::simulation::simulation::run") %>%
  group_by(size) %>%
  summarize(run_time = mean(median_dur) / 1e9) %>%
  mutate(rtr = 129600 / run_time) %>%
  mutate(speedup = run_time[size == 1] / run_time) %>%
  mutate(name = "Prototype 3%")
timings_5pct <- read_binary_tracing_files(
  "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/scaling-runtimes/instrument/rvr-5.0pct",
) %>%
  filter(func == "rust_q_sim::simulation::simulation::run") %>%
  group_by(size) %>%
  summarize(run_time = mean(median_dur) / 1e9) %>%
  mutate(rtr = 129600 / run_time) %>%
  mutate(speedup = run_time[size == 1] / run_time) %>%
  mutate(name = "Prototype 5%")
timings_10pct <- read_binary_tracing_files(
  "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/scaling-runtimes/instrument/rvr-10.0pct",
) %>%
  filter(func == "rust_q_sim::simulation::simulation::run") %>%
  group_by(size) %>%
  summarize(run_time = mean(median_dur) / 1e9) %>%
  mutate(rtr = 129600 / run_time) %>%
  mutate(speedup = run_time[size == 1] / run_time) %>%
  mutate(name = "Prototype 10%")
timings_qsim_10pct <- read_matsim_tracing_files("/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/rvr-v1.4-10pct/matsim-benchmark") %>%
  mutate(run_time = duration / 1e9) %>%
  mutate(rtr = 129600 / run_time) %>%
  select(size, run_time, rtr) %>%
  mutate(speedup = run_time[size == 1] / run_time) %>%
  mutate(name = "QSim 10%")

combined_timings <- bind_rows(
  timings_0pct,
  timings_1pct,
  timings_3pct,
  timings_5pct,
  timings_10pct,
  timings_qsim_10pct
)

max_rtr <- combined_timings %>%
  group_by(name) %>%
  filter(name == "Prototype 10%" |
           name == "Prototype 0%" |
           name == "QSim 10%") %>%
  filter(rtr == max(rtr)) %>%
  mutate(rtr = round(rtr, 0))
min_rtr <- combined_timings %>%
  group_by(name) %>%
  filter(name == "Prototype 10%" |
           name == "Prototype 0%" |
           name == "QSim 10%") %>%
  filter(rtr == min(rtr)) %>%
  mutate(rtr = round(rtr, 0))

combined_timings <- combined_timings %>%
  group_by(name) %>%
  mutate(ref_run_time = run_time[size == 1]) %>%
  mutate(speedup = ref_run_time / run_time) %>%
  mutate(efficiency = speedup / size) %>%
  ungroup() %>%
  mutate(name = fct_relevel(name, c("Prototype 0%", "Prototype 1%", "Prototype 3%", "Prototype 5%", "Prototype 10%", "QSim 10%")))

runtime_10pct_1 <- timings_10pct %>%
  filter(size == 1) %>%
  pull(run_time) %>%
  as.numeric()
runtime_1pct_1 <- timings_1pct %>%
  filter(size == 1) %>%
  pull(run_time) %>%
  as.numeric()

theoretical_rtr <- function(min_val, p) {
  t_cmp <- min_val / p
  N_nb <- 2 * (8 * sqrt(p) - 1) * (sqrt(p) - 1) / p
  #N_nb <- 10 / (1 + exp(-0.2 * (p - 32.5)))
  #bla <- p - 1
  # N_nb <- min(20, bla)
  t_lt <- 2 * N_nb * 1e-6 * 129600
  rtr <- 129600 / (t_cmp + t_lt)
  return(rtr)
}

p1 <- ggplot(combined_timings, aes(x = size, y = rtr, color = as.factor(name))) +
  geom_line() +
  geom_point() +
  #stat_function(fun = function(x) { theoretical_rtr(runtime_10pct_1, x) }, color = red(), linetype = "dotted") +
  #stat_function(fun = function(x) { theoretical_rtr(runtime_1pct_1, x) }, color = "#F0A202", linetype = "dotted") +
  geom_label(data = min_rtr, aes(label = rtr), vjust = -0.3, hjust = 0.35, show.legend = FALSE) +
  geom_label(data = max_rtr, aes(label = rtr), vjust = 1.3, hjust = 0.2, show.legend = FALSE) +
  scale_x_log10(labels = trans_format("log10", math_format(10^.x))) +  # Format x-axis
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +  # Format x-axis
  scale_color_manual(values = palette2()) +
  xlab("# Processes") +
  ylab("Real Time Ratio") +
  labs(color = "Setup") +
  ggtitle("Real Time Ratio for benchmark runs") +
  theme_light(base_size = 10) +
  theme(legend.position = "inside",
        legend.justification = c(0.98, 0.02),
        legend.box.background = element_rect(fill = "#FFFFFF", color = "gray"),
        #text = element_text(family="Roboto Slab"),
        plot.title = element_text(size = 12, face = "bold"),  # Set plot title to 8pt
        axis.title.x = element_text(size = 10),  # Set x-axis title to 8pt
        axis.title.y = element_text(size = 10),  # Set y-axis title to 8pt
        axis.text.x = element_text(size = 10),  # Set x-axis text to 8pt
        axis.text.y = element_text(size = 10),  # Set y-axis text to 8pt
        legend.title = element_text(size = 10, face = "bold"),  # Set legend title to 8pt
        legend.text = element_text(size = 10))  # Sets legend at the bottom of the plot

max_speedup <- combined_timings %>%
  group_by(name) %>%
  filter(name != "Prototype 0%") %>%
  filter(speedup == max(speedup)) %>%
  mutate(speedup = round(speedup, 1))

p2 <- ggplot(combined_timings, aes(x = size, y = speedup, color = name)) +
  geom_line() +
  geom_point() +
  geom_label(data = max_speedup, aes(label = speedup), vjust = 1.3, hjust = 0.5, show.legend = FALSE) +
  scale_x_log10(labels = trans_format("log10", math_format(10^.x))) +  # Format x-axis
  scale_color_manual(values = palette2()) +
  xlab("Number of processes") +
  ylab("Speedup") +
  labs(color = "Setup") +
  ggtitle("Speedups for benchmark runs") +
  theme_light(base_size = 10) +
  theme(legend.position = "right",
        legend.justification = c(0.98, 0.98),
        legend.box.background = element_rect(fill = "#FFFFFF", color = "gray"),
        #text = element_text(family="Roboto Slab"),
        plot.title = element_text(size = 12, face = "bold"),  # Set plot title to 8pt
        axis.title.x = element_text(size = 10),  # Set x-axis title to 8pt
        axis.title.y = element_text(size = 10),  # Set y-axis title to 8pt
        axis.text.x = element_text(size = 10),  # Set x-axis text to 8pt
        axis.text.y = element_text(size = 10),  # Set y-axis text to 8pt
        legend.title = element_text(size = 10, face = "bold"),  # Set legend title to 8pt
        legend.text = element_text(size = 10))  # Sets legend at the bottom of the plot
p12 <- p1 +
  p2 +
  plot_layout(ncol = 2, guides = "collect") & theme(legend.position = "right", legend.box.background = element_rect(fill = "#FFFFFF", color = "gray"))
p12
ggsave("rtr-speedup.pdf", plot = p12, device = "pdf", width = 240, height = 100, units = "mm")
ggsave("rtr-speedup.png", plot = p12, device = "png", width = 240, height = 100, units = "mm")

work <- tracing_10pct %>%
  filter(func %in% work_filter) %>%
  pivot_wider(names_from = func, values_from = duration) %>%
  mutate(duration = `Finish activities` +
    `Teleport` +
    `Move nodes` +
    `Move links`) %>%
  mutate(phase = "Waiting") %>%
  group_by(sim_time, size, phase) %>%
  summarize(dur = max(duration) - min(duration), .groups = 'drop')

comm <- tracing_10pct %>%
  filter(func %in% comm_filter) %>%
  pivot_wider(names_from = func, values_from = duration) %>%
  #mutate(duration = Receive + Send + Handle) %>%
  mutate(duration = Receive) %>%
  mutate(phase = "Max. Communication") %>%
  group_by(sim_time, size, phase) %>%
  summarize(dur = max(duration), .groups = 'drop')

work_comm <- bind_rows(work, comm) %>%
  filter(size %in% c(16, 64, 1024)) %>%
  pivot_wider(names_from = phase, values_from = dur) %>%
  mutate(`Msg. exchange` = `Max. Communication` - `Waiting`) %>%
  pivot_longer(cols = c("Max. Communication", "Waiting", "Msg. exchange"), names_to = "phase", values_to = "dur") %>%
  mutate(sim_time_posix = as.POSIXct(sim_time, origin = "1970-01-01", tz = "UTC")) %>%
  mutate(phase = factor(phase, levels = c("Waiting", "Max. Communication", "Msg. exchange"))) %>%
  arrange(phase)

p <- ggplot(work_comm, aes(sim_time_posix, dur / 1e3, color = as.factor(phase))) +
  geom_point(shape = '.', alpha = 0.7) +
  facet_wrap(~size, scales = "fixed") +
  ylim(0, 700) +
  scale_x_datetime(labels = date_format("%H:%M", tz = "UTC")) +
  #scale_x_continuous(labels = scales::comma) +  # Format x-axis labels
  ggtitle("Timings for individual timesteps – 10% setup") +
  ylab("Duration [\u00B5s]") +
  xlab("Simulation Time") +
  labs(color = "Phase") +
  guides(color = guide_legend(override.aes = list(size = 2, alpha = 1.0, shape = 19))) +
  scale_color_manual(values = palette()) +
  theme_light(base_size = 10) +
  theme(legend.position = "inside",
        legend.justification = c(0.99, 0.95),
        legend.box.background = element_rect(fill = "#FFFFFF", color = "gray"),
        plot.title = element_text(size = 12, face = "bold"),  # Set plot title to 8pt
        axis.title.x = element_text(size = 10),  # Set x-axis title to 8pt
        axis.title.y = element_text(size = 10),  # Set y-axis title to 8pt
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),  # Set x-axis text to 8pt
        axis.text.y = element_text(size = 10),  # Set y-axis text to 8pt
        legend.title = element_text(size = 10, face = "bold"),  # Set legend title to 8pt
        legend.text = element_text(size = 10))  # Sets legend at the bottom of the plot
ggsave("work-wait-hlrn.pdf", plot = p, device = "pdf", width = 210, height = 100, units = "mm")
ggsave("work-wait-hlrn.png", plot = p, device = "png", width = 210, height = 100, units = "mm")
p

acc_runtimes <- tracing_10pct %>%
  filter(func %in% work_filter | func %in% comm_filter) %>%
  group_by(size, func) %>%
  summarize(mean_dur = mean(duration), sum_dur = sum(duration))
ordered_data <- acc_runtimes %>%
  mutate(func = fct_relevel(func, function_filter))

p1 <- ggplot(ordered_data, aes(x = as.factor(size), y = mean_dur / 1e3, fill = as.factor(func))) +
  geom_bar(stat = "identity", position = "fill") +
  xlab("Number of processes") +
  ylab("Mean relative duration") +
  labs(fill = "Phase") +
  ggtitle("Algorithm phases – 10% sample") +
  scale_fill_manual(values = c(yellows(), blues())) +
  theme_light(base_size = 10) +
  theme(
    legend.box.background = element_rect(fill = "#FFFFFF", color = "gray"),
    plot.title = element_text(size = 12, face = "bold"),  # Set plot title to 8pt
    axis.title.x = element_text(size = 10),  # Set x-axis title to 8pt
    axis.title.y = element_text(size = 10),  # Set y-axis title to 8pt
    axis.text.x = element_text(size = 10),  # Set x-axis text to 8pt
    axis.text.y = element_text(size = 10),  # Set y-axis text to 8pt
    legend.title = element_text(size = 10, face = "bold"),  # Set legend title to 8pt
    legend.text = element_text(size = 10))  # Sets legend at the bottom of the plot

acc_runtimes_dry <- tracing_0pct %>%
  filter(func %in% work_filter | func %in% comm_filter) %>%
  group_by(size, func) %>%
  summarize(mean_dur = mean(duration), sum_dur = sum(duration))
ordered_data_dry <- acc_runtimes_dry %>%
  mutate(category = if_else(size > 256, "large", "small")) %>%
  mutate(func = fct_relevel(func, function_filter))

p2 <- ggplot(ordered_data_dry, aes(x = as.factor(size), y = mean_dur / 1e3, fill = as.factor(func))) +
  geom_bar(stat = "identity", position = "stack") +
  xlab("Number of processes") +
  ylab("Mean duration [\u00B5s]") +
  labs(fill = "Phase") +
  ggtitle("Algorithm phases – 0% sample") +
  scale_fill_manual(values = c(yellows(), blues())) +
  theme_light(base_size = 8) +
  theme_light(base_size = 10) +
  theme(
    legend.justification = c(0.5, 0.5),
    plot.title = element_text(size = 12, face = "bold"),  # Set plot title to 8pt
    axis.title.x = element_text(size = 10),  # Set x-axis title to 8pt
    axis.title.y = element_text(size = 10),  # Set y-axis title to 8pt
    axis.text.x = element_text(size = 10),  # Set x-axis text to 8pt
    axis.text.y = element_text(size = 10),  # Set y-axis text to 8pt
    legend.title = element_text(size = 10, face = "bold"),  # Set legend title to 8pt
    legend.text = element_text(size = 10))  # Sets legend at the bottom of the plot
combined_plot <- p1 +
  p2 +
  plot_layout(ncol = 2, guides = "collect") & theme(legend.position = "right", legend.box.background = element_rect(fill = "#FFFFFF", color = "gray"))
combined_plot
ggsave("acc_runtimes.pdf", plot = combined_plot, device = "pdf", width = 240, height = 100, units = "mm")
ggsave("acc_runtimes.png", plot = combined_plot, device = "png", width = 240, height = 100, units = "mm")

neighbor_data <- read_csv("/Users/janek/hlrn/strong-scaling/rvr/rvr-0.0pct/input/rvr-0.0pct.neighbors.csv") %>%
  group_by(size) %>%
  summarize(mean_neighbors = mean(neighbors), median_neighbors = median(neighbors), max_neighbors = max(neighbors), min_neighbors = min(neighbors), .groups = "drop")

receive_by_neighbor <- ordered_data_dry %>%
  filter(func == "Receive") %>%
  filter(size > 1) %>%
  left_join(neighbor_data, join_by(size)) %>%
  mutate(duration_by_max_neighbors = mean_dur / max_neighbors, duration_by_mean_neighbors = mean_dur / mean_neighbors) %>%
  select(size, mean_neighbors, max_neighbors, duration_by_mean_neighbors, duration_by_max_neighbors)
receive_by_neighbor

