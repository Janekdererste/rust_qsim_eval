blue_to_red <- function() {
  c("#0079ff","#859efc","#bfc6f7", "#f1f1f1","#ffb7bd", "#ff788d", "#ff0060")
}

neon <- function() {
  c(blue(),pink(),yellow(), green(), gray())
}



red <- function() {
  "#ff0060"
}

gray <- function() {
  "#333333"
}

green <- function() {
  "#80D39B"
}

orange <- function() {
  "#f5b700"
}

pink <- function() {
  "#DE0D92"
}

blue <- function() {
  "#2E86AB"
}

yellow <- function() {
  "#F5F749"
}

theme <- function(ggplot_object) {
  ggplot_object +
    scale_fill_manual(values = blue_to_red()) +
    scale_color_manual(values = blue_to_red()) +
    theme_light()
}