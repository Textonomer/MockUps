#
# Run this file once to clean the provided data set `./data/ph1-meta.csv` and
# save the nice results to `./data/clean.rds`
#
# Make sure to correctly specific the `data_dir` parameter in `utils.r`
#
# Unnecessary to run this code if you already have the file `./data/clean.rds`
#

library(tidyverse)
library(stringr)
source('utils.r')

fix_missing_encoding <- function(x) {
  lower_x <- tolower(x)
  x[lower_x == "unknown" | lower_x == "xxxx" | lower_x == "?"] <- NA
  x
}

cap_first <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

remove_q_mark <- function(x) str_replace_all(x, "\\?", "")

first_n_words <- function(string, n = 8) {
  words <- str_split(string, " ") %>%
    unlist() %>%
    magrittr::extract(1:min(n, length(.))) %>%
    paste(collapse = " ")
}

first_lang <- "(?!:[^a-z]*)[a-z]{3}"
last_city <- "(?!:.*)(Saint-|St\\. )?[A-Z][a-z]+$"
cs_name <- "[A-Z][a-z]+, [A-Z][a-z]+"
full_name <- "[A-Z][a-z]+ [A-Z][a-z]+"

read_csv(paste0(data_dir, "ph1-meta.csv")) %>%
  mutate_all(fix_missing_encoding) %>%
  mutate(Language = Languages %>%
           str_extract(first_lang) %>%
           recode("eng" = "English",
                  "lat" = "Latin",
                  "ita" = "Italian",
                  "wel" = "Welsh",
                  "sco" = "Scottish",
                  "fre" = "French",
                  "dut" = "Dutch",
                  "get" = "German",
                  "heb" = "Hebrew"),
         Location = CleanedUpPubPlaces %>%
           cap_first() %>%
           remove_q_mark() %>%
           str_extract(last_city),
         Author = ListedAuthors %>%
           str_extract(cs_name),
         Publisher = Publishers %>%
           str_replace_all("By ", "") %>%
           str_replace_all("Be ", "") %>%
           str_replace_all("In ", "") %>%
           str_extract(full_name),
         NumWords = plyr::round_any(WordCount, 5000) %>%
           format(trim = TRUE, big.mark = ",")) %>%
  select(Year, Location, Author, Publisher, WordCount, Language, NumWords) %>%
  write_rds(paste0(data_dir, "clean.rds"))
