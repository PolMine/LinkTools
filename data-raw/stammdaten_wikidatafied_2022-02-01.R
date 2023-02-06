# prepare and select Stammdaten (2023-02-06)

library(dplyr)
library(tidyr)
library(data.table)

stammdaten_file <- read.csv("~/lab/gitlab/bt_stammdaten/stammdaten_wikidatafied_2022-02-01.csv")

stammdaten_wikidatafied_2022_02_01_min <- stammdaten_file |>
  filter(lp %in% c(13, 14)) |>
  select(c(first_name, name_adel, name_praefix, family_name, party_by_lp, lp, parliamentary_group, QID)) |>
  mutate(role = "mp") |>
  mutate(speaker_full_name = trimws(paste(first_name, name_adel, name_praefix, family_name))) |>
  mutate(speaker_full_name = gsub("\\s+", " ", speaker_full_name)) |>
  mutate(lp = as.character(lp)) |>
  separate_rows(parliamentary_group, sep = "\\|") |>
  select(speaker_full_name, party_by_lp, lp, role, QID) |>
  rename("party" = "party_by_lp", 
                "speaker" = "speaker_full_name", 
                "wikidata_id" = "QID") |>
  as.data.table()

save(stammdaten_wikidatafied_2022_02_01_min, file = "~/lab/github/LinkTools/data/stammdaten_wikidatafied_2022_02_01_min.rda")
