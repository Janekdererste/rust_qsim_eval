#tidyverse https://www.tidyverse.org/ which is the stuff we use to wrangle and plot our data
library(tidyverse)
# json library recommendet by tidyverse https://cran.r-project.org/web/packages/jsonlite/vignettes/json-aaquickstart.html
library(jsonlite)

source("../colors.R")
source("../parsing.R")
source("../tracing.R")
source("routing_utils.R")

get_label <- function(input_string) {
  # Extrahiere den Substring nach dem letzten ::
  substring_after_last_double_colon <- sub(".+::([^:]+)$", "\\1", input_string)

  # Ausgabe des extrahierten Substrings
  return(substring_after_last_double_colon)
}

traces <- load_csv_instrument("../../../../assets/hlrn", num_cores = 16, node_count_filter = 256)

filtered_data <- filter_by_func(traces, routing_filter)

ggplot(filtered_data, aes(x = factor(rank), y = duration/1e6)) +
  geom_boxplot() +
  facet_grid(. ~ func, labeller = as_labeller(get_label)) +
  labs(title = "Boxplot", x = "Rank", y = "Duration")