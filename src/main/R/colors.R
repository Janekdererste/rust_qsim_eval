blue_to_red <- function() {
  c("#0079ff","#859efc","#bfc6f7", "#f1f1f1","#ffb7bd", "#ff788d", "#ff0060")
}

blue <- function() {
  "#0079ff"
}

red <- function() {
  "#ff0060"
}

gray <- function() {
  "#f1f1f1"
}

theme <- function(ggplot_object) {
  ggplot_object +
    scale_fill_manual(values = blue_to_red()) +
    scale_color_manual(values = blue_to_red()) +
    theme_light()
}