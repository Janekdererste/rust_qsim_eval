#tidyverse https://www.tidyverse.org/ which is the stuff we use to wrangle and plot our data
library(tidyverse)

source("./src/main/R/tracing.R")
source("./src/main/R/colors.R")

traces_4 <- load_rust_tracing_data("/Users/janek/Documents/rust_q_sim/berlin/output-1pct/size-4", num_cores = 8)

times <- traces_4 %>%
  select(sim_time, rank, size, func, func_name, duration) %>%
  filter(func == "rust_q_sim::simulation::network::sim_network::move_nodes" |
           func == "rust_q_sim::simulation::network::sim_network::move_links" |
           func == "rust_q_sim::simulation::messaging::communication::communicators::send_receive_vehicles" |
           func == "rust_q_sim::simulation::messaging::communication::communicators::receive_msgs")

p <- ggplot(data = times, aes(y = duration, x = factor(rank), fill = func_name)) +
  ylim(0, 0.7e5) +
  geom_boxplot(outlier.shape = NA) +
  ggtitle("Execution times by partition.") +
  xlab("Partition Rank") +
  ylab("duration [ns]") +
  scale_fill_manual(values = neon()) +
  scale_color_manual(values = neon()) +
  theme_light()
p


traces_combined <- load_rust_tracing_data("/Users/janek/Documents/rust_q_sim/berlin/output-1pct", num_cores = 8)

main_functions <- traces_combined %>%
  filter(func == "rust_q_sim::simulation::network::sim_network::move_nodes" |
           func == "rust_q_sim::simulation::network::sim_network::move_links" |
           func == "rust_q_sim::simulation::messaging::communication::communicators::receive_msgs")

medians <- main_functions %>%
  group_by(func_name, size) %>%
  summarise(average_duration = mean(duration) / 1e3)

ggplot(main_functions, aes(factor(size), y = (duration / 1e3), fill = as.factor(func_name))) +
  ylim(0, 500) +
  xlab("Number of Cores") +
  ylab("Median Duration [\u00B5s]") +
  scale_fill_manual(values = neon()) +
  theme_light() +
  geom_boxplot()

ggplot(medians, aes(x = factor(size), y = average_duration, fill = as.factor(func_name))) +
  geom_bar(stat = "identity", position = "stack") +
  xlab("Number of Cores") +
  ylab("Mean Duration [\u00B5s]") +
  scale_fill_manual(values = neon()) +
  scale_color_manual(values = neon()) +
  theme_light()

terminate_and_wakeup <- traces_combined %>%
  filter(
    func == "rust_q_sim::simulation::simulation::wakeup" |
      func == "rust_q_sim::simulation::simulation::terminate_teleportation")

medians_taw <- terminate_and_wakeup %>%
  group_by(func_name, size) %>%
  summarize(average_duration = median(duration))

ggplot(medians_taw, aes(x = factor(size), y = average_duration, fill = as.factor(func_name))) +
  geom_bar(stat = "identity", position = "stack") +
  xlab("Number of Cores") +
  scale_fill_manual(values = neon()) +
  scale_color_manual(values = neon()) +
  theme_light()


#----------- Plot messaging times for one size configuration
rcv_msgs <- traces_combined %>%
  #filter(size == 12 | size size == 2) %>%
  filter(func == "rust_q_sim::simulation::messaging::communication::communicators::receive_msgs") %>%
  filter(sim_time != 0) %>%
  mutate(time_bin = cut_width(sim_time, width = 1800, boundary = 1.)) %>%
  group_by(time_bin, size, rank) %>%
  summarize(m_dur = median(duration), .groups = 'drop')

ggplot(rcv_msgs, aes(x = time_bin, y = m_dur / 1e3, group = rank, color = factor(rank), fill = factor(rank))) +
  geom_line() +
  facet_wrap(~size) +
  scale_color_viridis_d(name = "Rank") +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 24)]) +  # Adjust 'by' as needed
  xlab("Simulation Time") +
  ylab("median duration [\u00B5s]") +
  ggtitle("Execution Times for send_rcv_veh") +
  theme_light()

move_nodes <- traces_combined %>%
  #filter(size == 12 | size size == 2) %>%
  filter(func == "rust_q_sim::simulation::network::sim_network::move_nodes") %>%
  filter(sim_time != 0) %>%
  mutate(time_bin = cut_width(sim_time, width = 1800, boundary = 1.)) %>%
  group_by(time_bin, size, rank) %>%
  summarize(m_dur = median(duration), .groups = 'drop')

ggplot(move_nodes, aes(x = time_bin, y = m_dur / 1e3, group = rank, color = factor(rank), fill = factor(rank))) +
  geom_line() +
  facet_wrap(~size) +
  scale_color_viridis_d(name = "Rank") +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 24)]) +  # Adjust 'by' as needed
  xlab("Simulation Time") +
  ylab("median duration [\u00B5s]") +
  ggtitle("Execution Times for network::move_nodes") +
  theme_light()

move_links <- traces_combined %>%
  #filter(size == 12 | size size == 2) %>%
  filter(func == "rust_q_sim::simulation::network::sim_network::move_links") %>%
  filter(sim_time != 0) %>%
  mutate(time_bin = cut_width(sim_time, width = 1800, boundary = 1.)) %>%
  group_by(time_bin, size, rank) %>%
  summarize(m_dur = median(duration), .groups = 'drop')

ggplot(move_links, aes(x = time_bin, y = m_dur / 1e3, group = rank, color = factor(rank), fill = factor(rank))) +
  geom_line() +
  facet_wrap(~size) +
  scale_color_viridis_d(name = "Rank") +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 24)]) +  # Adjust 'by' as needed
  xlab("Simulation Time") +
  ylab("median duration [\u00B5s]") +
  ggtitle("Execution Times for network::move_links") +
  theme_light()
