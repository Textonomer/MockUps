library(tidyverse)

# be sure to set this correctly!
data_dir <- "C:/Users/alex/Desktop/textonomer/treemap/data/"

tm_file_name <- function(level_1, level_2, level_3) {
  c(level_1, level_2, level_3) %>%
    tolower() %>%
    paste(collapse = "_") %>%
    paste0("./data/", ., ".rds")
}

load_hctreemap <- function(...) {
  tm_file_name(...) %>% read_rds()
}

`%notin%` <- function (x, table) match(x, table, nomatch = 0L) == 0L

feat <- c("Year", "Location", "Language", "NumWords", "Author", "Publisher")
num_words_name <- "Document Size (word count)"
names(feat) <- feat
names(feat)[which(feat == "NumWords")] <- num_words_name

# logic to specify options in Shiny drop down menus
remaining <- function(choice1, choice2 = NULL) {

  if (!is.null(choice2)) {
    if (choice2 == "None") return(c("None" = "None"))
    remain <- setdiff(feat, c(choice1, choice2))
  } else {
    remain <- setdiff(feat, c(choice1))
  }

  remain <- "None" %>% append(remain)
  names(remain) <- remain

  if ("NumWords" %in% remain) {
    names(remain)[which(remain == "NumWords")] <- num_words_name
  }

  remain
}
