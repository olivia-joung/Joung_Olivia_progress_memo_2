## Load packages
library(tidyverse)
library(janitor)
library(naniar)
library(magick)

## Load data
clues <- read_tsv("data/clues.tsv")
puzzles <- read_tsv("data/puzzles.tsv")
similar <- read_tsv("data/similar.tsv")
stats <- read_tsv("data/stats.tsv")
dictionary <- read_csv("data/english dictionary.csv")

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

clues_similar <- clues_edited |> 
  left_join(similar_edited)

# Cleaning puzzles
puzzles_edited <- puzzles |> 
  mutate(
    pubid_year = str_remove_all(xdid, "-\\d{2}-\\d{2}"),
    weekday = wday(Date, label = TRUE)) |> 
  relocate(pubid_year, .before = xdid) |> 
  arrange(Date) 

# Cleaning stats
stats_edited <- stats |> 
  filter(NumXd >= 1 & !is.na(NumXd)) |> 
  arrange(year) |>
  mutate(
    pubid_year = paste0(pubid, year)
  ) |> 
  relocate(pubid_year, .before = pubid)

# Joining stats with clues and puzzles 
clues_joined <- clues_edited |> 
  left_join(stats_edited |> select(pubid, year, weekday, Size, NumXd),
            join_by(pubid, year),
            relationship = "many-to-many")

puzzles_joined <- puzzles_edited |> 
  left_join(stats_edited |> select(pubid_year, weekday, Size, Editor, Copyright,
                                   NumXd),
            join_by(pubid_year, weekday, Size, Editor, Copyright),
            relationship = "many-to-many") |> 
  filter(!is.na(NumXd))

#clues_joined |> 
#  left_join(puzzles_joined, relationship = "many-to-many")

## EDA

# how many puzzles per year? - data will be skewed
puzzles_number_plot <- puzzles_joined |> 
  mutate(
    year = as.integer(str_remove_all(pubid_year, "[a-zA-Z]+"))
  ) |> 
  relocate(year, .before = pubid_year) |> 
  group_by(year) |>
  summarize(
    n = n()
  ) |> 
  ggplot(aes(year)) +
  geom_density() +
  labs(
    title = "Proportion of Puzzles by Year",
    x = "Year",
    y = "Density"
  )

ggsave(filename = "plots/puzzles_number_plot.png", plot = puzzles_number_plot)
  
# most common clue?
common_clues <- clues_joined |> 
  mutate(
    answer = as_factor(answer)
  ) |> 
  group_by(answer) |> 
  summarize(
    n = n()
  ) |> 
  arrange(desc(n)) |> 
  slice_max(n, n = 10)

common_clues_table <- knitr::kable(common_clues, format = "html") |>
  kable_styling() |>
  save_kable("plots/common_clues_table.png")

# three-letter/four-letter combos
three_letter <- clues_joined |> 
  filter(str_length(answer) == 3) |>
  mutate(
    answer = as_factor(answer)
  ) |>
  group_by(answer) |> 
  summarize(
    n = n()
  ) |> 
  arrange(desc(n)) |> 
  slice_max(n, n = 10)

three_letter_table <- knitr::kable(three_letter, format = "html") |>
  kable_styling() |>
  save_kable("plots/three_letter_table.png")

four_letter <- clues_joined |> 
  filter(str_length(answer) == 4) |>
  mutate(
    answer = as_factor(answer)
  ) |>
  group_by(answer) |> 
  summarize(
    n = n()
  ) |> 
  arrange(desc(n)) |> 
  slice_max(n, n = 10)

four_letter_table <- knitr::kable(four_letter, format = "html") |>
  kable_styling() |>
  save_kable("plots/four_letter_table.png")

# dictionary stuff
dictionary_edited <- dictionary |> 
  filter(!is.na(pos)) |> 
  mutate(
    answer = as_factor(toupper(word))
  ) 

# only proper nouns
clues_joined_edited <- clues_joined |> 
  left_join(dictionary_edited |> select(answer, pos), 
            join_by(answer), 
            relationship = "many-to-many") 

clues_joined_edited |> 
  filter(is.na(pos))


















