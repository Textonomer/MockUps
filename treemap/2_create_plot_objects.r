#
# This file creates the treemaps of interest and saves them to disk. These plots
# get loaded from disk and then rendered in the Shiny application.
#

library(tidyverse)
library(stringr)
library(highcharter)
source('utils.r')

clean <- read_rds(paste0(data_dir, "clean.rds"))

# turns a dataframe into a highcharter plot object with treemap behavior
# this function should eventually appear in highcharter as `hctreemap2`. as of
# July 7, 2017 both the CRAN and development versions of highcharter had major
# bugs in their treemap creation wrappers. i've written this into a PR and the
# official package should hopefully be fixed soon.
treemap4 <- function(data, group_vars, size_var, color_var = NULL, ...) {

  stopifnot(is.data.frame(data))
  stopifnot(is.character(group_vars))
  stopifnot(is.character(size_var))
  if (!is.null(color_var)) stopifnot(is.character(color_var))

  # fair warning: lots of dplyr NSE
  group_syms <- rlang::syms(group_vars)
  size_sym <- rlang::sym(size_var)
  color_sym <- rlang::sym(ifelse(is.null(color_var), size_var, color_var))

  if (data %>%
      select(!!!group_syms) %>%
      map(unique) %>%
      unlist() %>%
      anyDuplicated()) stop("Treemap data uses same label at multiple levels.")

  data <- data %>% mutate_at(group_vars, as.character)

  name_cell <- function(..., depth) paste0(list(...), 1:depth, collapse = "")

  data_at_depth <- function(depth) {
    data %>%
      group_by(!!!group_syms[1:depth]) %>%
      summarise(
        value = sum(!!size_sym),
        colorValue = sum(!!color_sym)
      ) %>%
      ungroup() %>%
      mutate(
        name = !!group_syms[[depth]],
        level = depth
      ) %>%
      mutate_at(group_vars, as.character()) %>%
      {
        if (depth == 1) mutate(., id = paste0(name, 1))
        else {
          mutate(
            .,
            parent = pmap_chr(
              list(!!!group_syms[1:depth - 1]),
              name_cell,
              depth = depth - 1),
            id = paste0(parent, name, depth)
          )
        }
      }
  }

  treemap_df <- 1:length(group_vars) %>%
    map(data_at_depth) %>%
    bind_rows()

  data_list <- treemap_df %>%
    highcharter::list_parse() %>%
    purrr::map(~.[!is.na(.)])

  # print total number of cells in treemap. use for heuristics.
  print(length(data_list))

  colorVals <- treemap_df %>%
    filter(level == length(group_vars)) %>%
    pull(colorValue)

  highchart() %>%
    hc_add_series(
      type = "treemap",
      allowDrillToNode = TRUE,
      data = data_list,
      layoutAlgorithm = "squarified",
      levelIsConstant = FALSE,
      levels = list(
        list(level = 1, dataLabels = list(enabled = TRUE)),
        list(level = 2, dataLabels = list(enabled = FALSE)),
        list(level = 3, dataLabels = list(enabled = FALSE))
      ),
      ...
    ) %>%
    hc_colorAxis(min = min(colorVals),
                 max = max(colorVals),
                 enabled = TRUE)
}

save_hctreemap <- function(df, level_1, level_2, level_3, max_cells = 1000) {

  hierarchy <- setdiff(c(level_1, level_2, level_3), "None")
  data <- df %>% select(WordCount, one_of(hierarchy))

  # subset data to limit bottom level of treemap to a maximum of max_cells cells
  data <- data %>%
    group_by(!!!rlang::syms(hierarchy)) %>%
    summarize(group_total = sum(WordCount)) %>%
    ungroup() %>%
    top_n(max_cells, group_total) %>%
    inner_join(df, ., hierarchy)

  na_is_most_common <- data[[level_1]] %>%
    table(useNA = "always") %>%
    sort() %>%
    names() %>%
    tail(1) %>%
    is.na()

  if (na_is_most_common) {
    # only consider cases with information of interest
    data <- data[complete.cases(data), ]
  } else {
    # rename NA values to something pretty and avoid uniqueness issues
    for (name in names(data)) {
      x <- data[[name]]
      data[[name]] <- ifelse(is.na(x), paste("Unknown", name), x)
    }
  }

  data %>%
    treemap4(hierarchy, "WordCount") %>%
    hc_title(text = "Treemap for exploring corpora", align = "left") %>%
    hc_subtitle(text = "The size of each cell is proportional to word count in
                that category", align = "left") %>%
    write_rds(tm_file_name(level_1, level_2, level_3))
}

# sanity checks
save_hctreemap(clean, "Year", "Language", "None")
load_hctreemap("Year", "Language", "None")

# notes: >3000 cells in a treemap won't really load. 1000 - 2000 an upper limit.

# create serialized plot objects
for (level_1 in feat) {
  for (level_2 in remaining(level_1)) {
    for (level_3 in remaining(level_1, level_2)) {
      print(c(level_1, level_2, level_3))
      save_hctreemap(clean, level_1, level_2, level_3, max_cells = 750)
    }
  }
}
