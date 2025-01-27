library(tidyverse)

source("./src/main/R/read_tracing_files.R")
source("./src/main/R/colors.R")

strong_scaling_01 <- read_binary_tracing_files("/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/scaling-runtimes/instrument-0.1/") %>%
  mutate(name = "00.1", index = 1)
strong_scaling_02 <- read_binary_tracing_files("/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/scaling-runtimes/instrument-0.2/") %>%
  mutate(name = "00.2", index = 2)
strong_scaling_04 <- read_binary_tracing_files("/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/scaling-runtimes/instrument-0.4/") %>%
  mutate(name = "00.4", index = 3)
strong_scaling_08 <- read_binary_tracing_files("/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/scaling-runtimes/instrument-0.8/") %>%
  mutate(name = "00.8", index = 4)
strong_scaling_16 <- read_binary_tracing_files("/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/scaling-runtimes/instrument-1.6/") %>%
  mutate(name = "01.6", index = 5)
strong_scaling_32 <- read_binary_tracing_files("/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/scaling-runtimes/instrument-3.2/") %>%
  mutate(name = "03.2", index = 6)
strong_scaling_64 <- read_binary_tracing_files("/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/scaling-runtimes/instrument-6.4/") %>%
  mutate(name = "06.4", index = 7)
strong_scaling_128 <- read_binary_tracing_files("/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/scaling-runtimes/instrument-12.8/") %>%
  mutate(name = "12.8", index = 8)
strong_scaling_256 <- read_binary_tracing_files("/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/scaling-runtimes/instrument-25.6/") %>%
  mutate(name = "25.6", index = 9)
strong_scaling_512 <- read_binary_tracing_files("/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/scaling-runtimes/instrument-51.2/") %>%
  mutate(name = "51.2", index = 10)
strong_scaling_1024 <- read_binary_tracing_files("/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/scaling-runtimes/instrument-102.4/") %>%
  mutate(name = "102.4", index = 11)
strong_scaling_2048 <- read_binary_tracing_files("/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/scaling-runtimes/instrument-204.8/") %>%
  mutate(name = "204.8", index = 12)

strong_scaling <- bind_rows(
  strong_scaling_01,
  strong_scaling_02,
  strong_scaling_04,
  strong_scaling_08,
  strong_scaling_16,
  strong_scaling_32,
  strong_scaling_64,
  strong_scaling_128,
  strong_scaling_256,
  strong_scaling_512,
  strong_scaling_1024,
  strong_scaling_2048
) %>%
  mutate(name = factor(name, levels = unique(name[order(index)]))) %>%
  group_by(name) %>%
  mutate(speedup = mean_dur[size == 1] / median_dur) %>%
  ungroup() %>%
  mutate(efficiency = speedup / size)

runtime_64_1 <- strong_scaling %>%
  filter(size == 1) %>%
  filter(name == "12.8") %>%
  pull(mean_dur) %>%
  as.numeric()

func <- function(p) {
  t_cmp <- runtime_64_1 * 1e-9 / p
  #N_nb <- 2 * (3 * sqrt(p) - 1) * (sqrt(p) - 1) / p
  N_nb <- 10 * 2
  t_lt <- N_nb * 1e-6 * 129600
  return(t_cmp + t_lt)
}

func_2 <- function(p) {
  return(20 * (1 - e^-p))
}

p <- ggplot(strong_scaling, aes(x = size, y = mean_dur / 1e9, color = name)) +
  geom_line() +
  geom_point() +
  stat_function(fun = func_2, color = "#F0A202") +
  #geom_label(data = speedup_001, aes(label = mean_dur / 1e9)) +
  labs(color = "Scenario Size in %") +
  scale_x_log10() +
  scale_y_log10() +
  ggtitle("Runtimes RVR-Scenario") +
  ylab("Runtime [s]") +
  xlab("# Processes") +
  theme_light()
p

p <- ggplot(strong_scaling, aes(x = size, y = speedup, color = name)) +
  geom_line() +
  geom_point() +
  #geom_label(data = speedup_001, aes(label = mean_dur / 1e9)) +
  labs(color = "Scenario Size in %") +
  scale_x_log10() +
  scale_y_log10() +
  ggtitle("Speedups RVR-Scenario") +
  ylab("Speedup") +
  xlab("# Processes") +
  theme_light()
p

p <- ggplot(strong_scaling, aes(x = size, y = efficiency, color = name)) +
  geom_line() +
  geom_point() +
  #geom_label(data = speedup_001, aes(label = mean_dur / 1e9)) +
  labs(color = "Scenario Size in %") +
  scale_x_log10() +
  scale_y_log10() +
  ggtitle("Efficiency RVR-Scenario") +
  ylab("Efficiency") +
  xlab("# Processes") +
  theme_light()
p


weak_scaling <- strong_scaling %>%
  filter(size == 2^(index + 3))

p <- ggplot(weak_scaling, aes(x = size, y = mean_dur / 1e9)) +
  geom_line() +
  geom_point() +
  geom_label(aes(label = name)) +
  scale_y_log10() +
  scale_x_log10() +
  ggtitle("Weak Scaling - RVR-Scenario") +
  ylab("Runtime [s]") +
  xlab("# Processes") +
  theme_light()
p