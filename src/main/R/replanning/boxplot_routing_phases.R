#tidyverse https://www.tidyverse.org/ which is the stuff we use to wrangle and plot our data
library(tidyverse)
# json library recommendet by tidyverse https://cran.r-project.org/web/packages/jsonlite/vignettes/json-aaquickstart.html
library(jsonlite)

source("./src/main/R/colors.R")
source("./src/main/R/parsing.R")
source("./src/main/R/tracing.R")
source("./src/main/R/replanning/routing_utils.R")

get_label <- function(input_string) {
  # Extrahiere den Substring nach dem letzten ::
  substring_after_last_double_colon <- sub(".+::([^:]+)$", "\\1", input_string)

  # Ausgabe des extrahierten Substrings
  return(substring_after_last_double_colon)
}

traces <- load_rust_tracing_data("./assets/hlrn-all/update-no-replan", num_cores = 16, node_count_filter = 256)

filtered_data <- filter_by_func(traces, c(DESERIALIZE_KEY, COLLECT_KEY, COMMUNICATION_ALL_KEY, COMMUNICATION_KEY, INSERTION_KEY, HANDLE_KEY)) %>%
  filter(sim_time != 0)

ggplot(filtered_data, aes(x = factor(rank), y = duration/1e6)) +
  geom_boxplot() +
  facet_grid(. ~ func, labeller = as_labeller(get_label)) +
  labs(title = "Boxplot", x = "Rank", y = "Duration")