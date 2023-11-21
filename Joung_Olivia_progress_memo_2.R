## Load packages
library(tidyverse)
library(janitor)
library(naniar)
library(lubridate)

## Load data
clues <- read_tsv("data/clues.tsv")
puzzles <- read_tsv("data/puzzles.tsv")
similar <- read_tsv("data/similar.tsv")


## Data Cleaning

# Cleaning/joining `clues` and `similar`
clues_edited <- clues |> 
  mutate(
    pubid_year = paste0(pubid, year)
  ) |> 
  relocate(pubid_year, .before = pubid) |> 
  arrange(year)

similar_edited <- similar |> 
  mutate(
    pubid_year = str_remove_all(xdid, "-\\d{2}-\\d{2}"),
    year = as.integer(str_remove_all(pubid_year, "[a-zA-Z]+"))
  ) |>
  relocate(c(year, pubid_year), .before = xdid) |> 
  group_by(pubid_year) |>
  summarize(
    reused_clues_mean = mean(reused_clues),
    reused_answers_mean = mean(reused_answers),
    total_clues_mean = mean(total_clues)
  )

clues_edited |> 
  left_join(similar_edited)

# Cleaning puzzles
puzzles_edited <- puzzles |> 
  mutate(
    pubid_year = str_remove_all(xdid, "-\\d{2}-\\d{2}"),
    weekday = wday(Date, label = TRUE)) |> 
  relocate(pubid_year, .before = xdid) |> 
  arrange(Date) 

puzzles_edited |> 
    count(pubid_year == "atc1997")
  

  


