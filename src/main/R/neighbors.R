library(tidyverse)
library(wesanderson)
source("./src/main/R/read_tracing_files.R")
source("./src/main/R/colors.R")

on_load <- function(data) {
  data %>%
    filter(bin_start > 0) %>%
    # filter(
    #   func == "rust_q_sim::simulation::messaging::communication::communicators::handle_msgs" |
    #     func == "rust_q_sim::simulation::messaging::communication::communicators::receive_msgs" |
    #     func == "rust_q_sim::simulation::messaging::communication::communicators::send_msgs" |
    #     func == "rust_q_sim::simulation::network::sim_network::move_links" |
    #     func == "rust_q_sim::simulation::network::sim_network::move_nodes" |
    #     func == "rust_q_sim::simulation::simulation::wakeup" |
    #     func == "rust_q_sim::simulation::simulation::terminate_teleportation") %>%
    filter(func == "rust_q_sim::simulation::messaging::communication::communicators::receive_msgs") %>%
    mutate(func_name = sub(".*::", "", func)) %>%
    mutate(sim_time = bin_start) %>%
    mutate(duration = median_dur) %>%
    mutate(func = func_name) %>%
    select(size, rank, func, duration)
}


neighbor_data <- read_csv("/Users/janek/hlrn/strong-scaling/rvr/rvr-0.0pct/input/rvr-0.0pct.neighbors.csv") %>%
  group_by(size) %>%
  summarize(mean_neighbors = mean(neighbors), median_neighbors = median(neighbors), max_neighbors = max(neighbors), min_neighbors = min(neighbors), .groups = "drop")

ggplot(neighbor_data, aes(x = size)) +
  geom_point(aes(y = min_neighbors), color = "grey") +
  geom_line(aes(y = min_neighbors), color = "grey") +
  geom_point(aes(y = max_neighbors), color = "red") +
  geom_line(aes(y = max_neighbors), color = "red") +
  geom_point(aes(y = mean_neighbors), color = "blue") +
  geom_line(aes(y = mean_neighbors), color = "blue") +
  geom_point(aes(y = median_neighbors), color = "green") +
  geom_line(aes(y = median_neighbors), color = "green") +
  theme_light()


neighbor_data <- read_neighbor_files("/Users/janek/hlrn/berlin-empty/output-with-tracing")

neighbors <- neighbor_data %>%
  group_by(size) %>%
  summarize(mean_neighbors = mean(neighbors), max_neighbors = max(neighbors), min_neighbors = min(neighbors), .groups = "drop")

traces <- read_binary_tracing_files("/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/berlin-v6.0-empty/output-with-tracing",
                                    on_load = on_load, parallel = TRUE) %>%
  group_by(size, func) %>%
  summarize(mean_dur = mean(duration), sum_dur = sum(duration), max_dur = max(duration))
messaging <- traces %>% filter(func == "receive_msgs")
joined <- neighbors %>%
  left_join(messaging, join_by("size")) %>%
  select(size, mean_neighbors, max_neighbors, mean_dur, max_dur) %>%
  mutate(duration_by_neighbor = mean_dur / max_neighbors / 1e3) %>%
  filter(size != 1) %>%
  pivot_longer(cols = c("max_neighbors", "mean_neighbors", "duration_by_neighbor"), values_to = "values", names_to = "class") %>%
  mutate(class = sapply(class, function(class_val) {
    if (class_val == "max_neighbors") return("Max. neighbors")
    if (class_val == "mean_neighbors") return("Avg. neighbors")
    if (class_val == "duration_by_neighbor") return("Dur. by max. neighbors")
  }))

p <- ggplot(joined, aes(x = size, y = values, color = class)) +
  geom_line() +
  geom_point() +
  geom_text(data = joined, aes(label = round(values, 1)), vjust = -0.5, hjust = -0.05) +
  scale_x_log10() +
  ggtitle("Avg. time to perform process synchronization and neighbor domains") +
  xlab("Processes") +
  ylab("Avg. execution time [\u00B5s] and #neighbors") +
  labs(color = "Colors") +
  scale_color_manual(values = wes_palette("Darjeeling1")) +
  theme_light()
ggsave("neighbors.pdf", plot = p, device = "pdf", width = 210, height = 100, units = "mm")
ggsave("neighbors.png", plot = p, device = "png", width = 210, height = 100, units = "mm")
p


p <- ggplot(neighbors, aes(x = size, y = mean_neighbors)) +
  geom_line(color = orange()) +
  geom_point(color = orange()) +
  geom_line(aes(y = max_neighbors), color = blue()) +
  geom_point(aes(y = max_neighbors), color = blue()) +
  geom_text(data = neighbors, aes(label = round(mean_neighbors, 1)), vjust = -0.5, hjust = -0.05) +
  scale_x_log10() +
  ggtitle("Mean and max number of neighbors") +
  xlab("Processes") +
  ylab("#Neighbors") +
  theme_light()
p