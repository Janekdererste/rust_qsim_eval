#tidyverse https://www.tidyverse.org/ which is the stuff we use to wrangle and plot our data
library(tidyverse)

source("./src/main/R/read_tracing_files.R")
source("./src/main/R/colors.R")

berlin_25pct_traces_wr <- read_binary_tracing_files("./assets/instrument-info-wr") %>%
  filter(func == "rust_q_sim::simulation::simulation::run") %>%
  group_by(size) %>%
  summarize(run_time = mean(median_dur) / 1e9) %>%
  mutate(rtr = 86400 / run_time) %>%
  mutate(name = "berlin-25%-full-routing")

berlin_25pct_traces_nr <- read_binary_tracing_files("./assets/instrument-info-nr") %>%
  filter(func == "rust_q_sim::simulation::simulation::run") %>%
  group_by(size) %>%
  summarize(run_time = mean(median_dur) / 1e9) %>%
  mutate(rtr = 86400 / run_time) %>%
  mutate(name = "berlin-25%-dry-router-updates-only")

#combined <- bind_rows(rvr_traces, berlin_in_link_traces, rvr_1pct_traces, berlin_traces, berlin_1pct_traces, matsim_traces, berlin_logging_traces)

combined <- bind_rows(berlin_25pct_traces_wr, berlin_25pct_traces_nr)
p <- ggplot(combined, aes(x = size, y = run_time, color = as.factor(name))) +
  geom_line() +
  geom_point() +
  scale_y_log10() +
  scale_x_log10() +
  geom_text(aes(label = round(run_time, 1)), vjust = -0.5, hjust = -0.05) +
  scale_color_manual(values = qualitative()) +
  xlab("Number of Cores") +
  ggtitle("Overall Runtime [s] on Intel® Xeon® Platinum 9242 Processor ") +
  labs(color = "Runs") +
  theme_light() +
  ggplot2::theme(
    axis.title.x = element_text(size = 14),  # Schriftgröße der x-Achsenbeschriftung ändern
    axis.title.y = element_text(size = 14),  # Schriftgröße der y-Achsenbeschriftung ändern
    axis.text.x = element_text(size = 14),   # Schriftgröße der x-Achsentickbeschriftungen ändern
    axis.text.y = element_text(size = 14),   # Schriftgröße der y-Achsentickbeschriftungen ändern
    plot.title = element_text(size = 14) ,    # Schriftgröße des Plot-Titels ändern
    legend.title = element_text(size = 14),    # Schriftgröße der Legende ändern
    legend.text = element_text(size = 14)     # Schriftgröße der Legendenbeschriftungen ändern
  )
p
ggsave("runtimes-hlrn.pdf", plot = p, device = "pdf", width = 297, height = 210, units = "mm")

p <- ggplot(combined, aes(x = size, y = rtr, color = as.factor(name))) +
  geom_line(size=1.2) +
  geom_point(size=1.2) +
  scale_y_log10() +
  scale_x_log10() +
  geom_text(aes(label = round(rtr, 1)), vjust = 1, hjust = -0.05, size=5) +
  scale_color_manual(values = qualitative()) +
  xlab("Number of Cores") +
  ggtitle("Real Time Ratio on Intel® Xeon® Platinum 9242 Processor ") +
  labs(color = "Runs") +
  theme_light() +
  ggplot2::theme(
    axis.title.x = element_text(size = 14),  # Schriftgröße der x-Achsenbeschriftung ändern
    axis.title.y = element_text(size = 14),  # Schriftgröße der y-Achsenbeschriftung ändern
    axis.text.x = element_text(size = 14),   # Schriftgröße der x-Achsentickbeschriftungen ändern
    axis.text.y = element_text(size = 14),   # Schriftgröße der y-Achsentickbeschriftungen ändern
    plot.title = element_text(size = 14) ,    # Schriftgröße des Plot-Titels ändern
    legend.title = element_text(size = 14),    # Schriftgröße der Legende ändern
    legend.text = element_text(size = 14) , # Schriftgröße der Legendenbeschriftungen ändern
    legend.position = "bottom"
  )
p
ggsave("rtr-hlrn.pdf", plot = p, device = "pdf", width = 297, height = 210, units = "mm")

# make a plot to compare matsim and rust results

rvr_matsim_comparison <- bind_rows(matsim_traces, rvr_traces) %>%
  filter(size <= 48) %>%
  group_by(name) %>%
  mutate(max_by_name = max(run_time)) %>%
  ungroup() %>%
  mutate(speedup = max_by_name / run_time) %>%
  select(size, run_time, speedup, rtr, name)

ggplot(rvr_matsim_comparison, aes(x = size, y = speedup, color = as.factor(name))) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = round(rtr, 1)), vjust = -0.5, hjust = -0.05) +
  scale_color_manual(values = qualitative()) +
  xlab("Number of Cores") +
  ggtitle("Speedup on Intel® Xeon® Platinum 9242 Processor ") +
  theme_light() +
  geom_abline(slope = 1, intercept = 0, color = "red")

ggplot(rvr_matsim_comparison, aes(x = size, y = rtr, color = as.factor(name))) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = round(rtr, 1)), vjust = -0.5, hjust = -0.05) +
  scale_color_manual(values = qualitative()) +
  xlab("Number of Cores") +
  ggtitle("RTR on one Intel® Xeon® Platinum 9242 Processor ") +
  theme_light()






