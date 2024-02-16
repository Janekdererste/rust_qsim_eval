#tidyverse https://www.tidyverse.org/ which is the stuff we use to wrangle and plot our data
library(tidyverse)
library(dplyr)
library(tidyr)

source("../colors.R")
source("../parsing.R")
source("../tracing.R")

traces <- load_csv_instrument("../../../../assets/hlrn", num_cores = 16)

runtimes <- traces %>%
  filter(sim_time != 0) %>% # remove the first entry
  group_by(size) %>%
  summarise(
    mean_duration_collecting = mean(tail(duration[func == COLLECT_KEY], -1), na.rm = TRUE) / 1e6,
    mean_duration_inserting = mean(tail(duration[func == INSERTION_KEY], -1), na.rm = TRUE) / 1e6,
    mean_duration_communicating = mean(tail(duration[func == COMMUNICATION_KEY], -2), na.rm = TRUE) / 1e6,
    mean_duration_handling = mean(tail(duration[func == HANDLE_KEY], -2), na.rm = TRUE) / 1e6
  )

data_long <- gather(runtimes, key = "duration_type", value = "duration_value", -size)

data_long$size <- as.factor(data_long$size)

# Gestapeltes Balkendiagramm erstellen
ggplot(data_long, aes(x = size, y = duration_value, fill = duration_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Gestapeltes Balkendiagramm",
       x = "Size",
       y = "Durations") +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3")) +
  theme_minimal()