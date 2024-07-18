library(tidyverse)

source("./src/main/R/read_tracing_files.R")
source("./src/main/R/colors.R")

weak_scaling <- read_binary_tracing_files("/Users/janek/hlrn/weak-scaling/berlin")
print(weak_scaling)

weak_dur_1 <- weak_scaling %>%
  filter(size == 1) %>%
  pull()
efficiency <- weak_scaling %>%
  mutate(efficiency = weak_dur_1 / mean_dur)

p <- ggplot(efficiency, aes(x = size, y = efficiency)) +
  geom_line() +
  geom_point() +
  scale_y_log10() +
  theme_light()
p
strong_scaling_01 <- read_binary_tracing_files("/Users/janek/hlrn/strong-scaling/berlin/berlin-0.1pct/") %>%
  mutate(name = "00.1")
strong_scaling_02 <- read_binary_tracing_files("/Users/janek/hlrn/strong-scaling/berlin/berlin-0.2pct/") %>%
  mutate(name = "00.2")
strong_scaling_04 <- read_binary_tracing_files("/Users/janek/hlrn/strong-scaling/berlin/berlin-0.4pct/") %>%
  mutate(name = "00.4")
strong_scaling_08 <- read_binary_tracing_files("/Users/janek/hlrn/strong-scaling/berlin/berlin-0.8pct/") %>%
  mutate(name = "00.8")
strong_scaling_16 <- read_binary_tracing_files("/Users/janek/hlrn/strong-scaling/berlin/berlin-1.6pct/") %>%
  mutate(name = "01.6")
strong_scaling_32 <- read_binary_tracing_files("/Users/janek/hlrn/strong-scaling/berlin/berlin-3.2pct/") %>%
  mutate(name = "03.2")
strong_scaling_64 <- read_binary_tracing_files("/Users/janek/hlrn/strong-scaling/berlin/berlin-6.4pct/") %>%
  mutate(name = "06.4")
strong_scaling_128 <- read_binary_tracing_files("/Users/janek/hlrn/strong-scaling/berlin/berlin-12.8pct/") %>%
  mutate(name = "12.8")
strong_scaling_256 <- read_binary_tracing_files("/Users/janek/hlrn/strong-scaling/berlin/berlin-25.6pct/") %>%
  mutate(name = "25.6")
strong_scaling_512 <- read_binary_tracing_files("/Users/janek/hlrn/strong-scaling/berlin/berlin-51.2pct/") %>%
  mutate(name = "51.2")
strong_scaling_1024 <- read_binary_tracing_files("/Users/janek/hlrn/strong-scaling/berlin/berlin-102.4pct/") %>%
  mutate(name = "102.4")
strong_scaling_2048 <- read_binary_tracing_files("/Users/janek/hlrn/strong-scaling/berlin/berlin-204.8pct/") %>%
  mutate(name = "202.8")

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
  strong_scaling_2048)

p <- ggplot(strong_scaling, aes(x = size, y = mean_dur / 1e9, color = as.factor(name))) +
  geom_line() +
  geom_point() +
  #geom_label(data = speedup_001, aes(label = mean_dur / 1e9)) +
  labs(color = "Scenario Size in %") +
  scale_x_log10() +
  scale_y_log10() +
  theme_light()
p

ggplot(speedup_001, aes(x = size, y = speedup)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  labs(title = "Speedup for 0.1% Sample", x = "# Processes", y = "Speedup") +
  theme_light(base_size = 14)