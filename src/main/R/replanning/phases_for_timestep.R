#tidyverse https://www.tidyverse.org/ which is the stuff we use to wrangle and plot our data
library(tidyverse)
# json library recommendet by tidyverse https://cran.r-project.org/web/packages/jsonlite/vignettes/json-aaquickstart.html
library(jsonlite)

source("./src/main/R/colors.R")
source("./src/main/R/parsing.R")
source("./src/main/R/tracing.R")
source("./src/main/R/replanning/routing_utils.R")

traces <- load_rust_tracing_data("assets/update_no_replan", num_cores = 16)
# saveRDS(traces, file = "assets/cluster-act/traces.rds")

# filter traces by time == 43200
traces_on_time <- traces %>%
  filter(sim_time == 900*4*17) %>%
  filter(func %in% c(COMMUNICATION_ALL_KEY, "rust_q_sim::simulation::simulation::wakeup", "rust_q_sim::simulation::simulation::move_links",
  "rust_q_sim::simulation::simulation::move_nodes", "rust_q_sim::simulation::simulation::terminate_teleportation"))

# group traces_on_time by rank and sum duration
traces_on_time_sum <- traces_on_time %>%
  group_by(rank) %>%
  summarise(duration = sum(duration))

ggplot() +
  geom_point(data = traces_on_time, aes(x = rank, y = duration/1e6, color=func_name)) +  # Punktdiagramm erstellen
  # geom_point(data = traces_on_time_sum, aes(x = rank, y = duration/1e6, color="Total")) +  # Linie erstellen
  labs(x = "Rank", y = "Duration", title = "Plot")
