## Load packages
library(tidyverse)
library(janitor)
library(naniar)

## Load data
clues <- read_tsv("data/clues.tsv")
puzzles <- read_tsv("data/puzzles.tsv")
select <- read_tsv("data/similar.tsv")


# data cleaning (joining)
clues_edited <- clues |> 
  mutate(
    pubid_year = paste0(pubid, year)
  ) |> 
  relocate(pubid_year, .before = pubid) |> 
  arrange(year)

clues_edited

distinct_pubid_year <- puzzles |> 
  mutate(
    pubid_year = str_remove_all(xdid, "-\\d{2}-\\d{2}")) |> 
  relocate(pubid_year, .before = xdid) |> 
  arrange(Date)

distinct_pubid_year
  
distinct_pubid_year |> 
  left_join(clues_edited) |> 
  select(pubid_year, answer, clue)


